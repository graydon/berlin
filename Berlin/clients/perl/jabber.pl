#!/usr/bin/perl-thread

# "Babel", a Jabber client for Berlin
# Copyright (C) 2000 Nathaniel Smith <nimrod@berlin-consortium.org>
# released under the terms of the GNU Lesser General Public License,
# version 2.1, or, at your discretion, any later version.

my $version = "0.2";

use Net::Jabber;

#$^W = 1;
$| = 1;
use Warsaw;

use Command_impl;

use strict;
my ($user, $pass) = @ARGV;

$pass || die "Usage: $0 user\@host/resource password";

my $con = undef;
my $kits = Warsaw::Utility::war_connect(`get-nameservice-ior`);
my $convs = {};

# Main
$user =~ m|(.*)@(.*)(/(.*))?|;
my $uid = $1;
my $host = $2;
my $resource = $4 || "default";
my $buf;
my $roster = {};

#my $main_window = start_gui();
jab_connect($uid, $host, $resource, $pass);
# $main_window->{log_in_out}->position(0);
#$main_window->{log_in_out}->removeForward($main_window->{log_in_out}
#					  ->size());
#$main_window->insertString(Warsaw::Utility::asc2uni("Logout"));
#$main_window->command(...)
chat_starter_window();
main_loop();

sub start_gui() {
# Roster
    # FIXME: make the roster stuff actually do something
    my $roster_box = $kits->{LayoutKit}->vbox();
    my $roster_area = $kits->{ToolKit}->group($roster_box);

# Buttons
    my $talk_label = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Chat"));
    $talk_label = $kits->{ToolKit}->rgb($talk_label);
    my $talk_b = $kits->{WidgetKit}->button($talk_label, undef);

    my $login_buf = $kits->{CommandKit}->text();
    $login_buf->insertString(Warsaw::Utility::asc2uni("Login"));
    my $login_label = $kits->{TextKit}->simpleViewer($login_buf);
    my $login_b = $kits->{WidgetKit}->button($login_label, undef);

    my $quit_label = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Quit"));
    $quit_label = $kits->{ToolKit}->rgb($quit_label);
    my $quit_b = $kits->{WidgetKit}->button($quit_label, undef);
    
    my $hfil = $kits->{LayoutKit}->hfil();
    my $buttons = $kits->{LayoutKit}->hbox();
    $buttons->append($hfil);
    $buttons->append($talk_b);
    $buttons->append($hfil);
    $buttons->append($login_b);
    $buttons->append($quit_b);
    $buttons->append($hfil);

# Status line
    my $buf = $kits->{CommandKit}->text();
    $buf->insertString(Warsaw::Utility::asc2uni("Babel $version"));
    my $status_area = $kits->{ToolKit}->rgb($kits->{TextKit}->simpleViewer($buf),
					    1,
					    1,
					    1,);
    
# Overall layout
    my $vbox = $kits->{LayoutKit}->vbox();
    my $vglue = $kits->{LayoutKit}->vglueFil(50);
    $vbox->append($roster_area);
    $vbox->append($vglue);
    $vbox->append($buttons);
    $vbox->append($vglue);
    $vbox->append($status_area);

    my $group = $kits->{ToolKit}->group($vbox);
    $group->appendController($roster_area);
    $group->appendController($talk_b);
    $group->appendController($login_b);
    $group->appendController($quit_b);

    my $window = $kits->{DesktopKit}->shell($group);

    my $quit_command = Warsaw::Utility::command {
	foreach my $name (keys %$convs) {
	    Warsaw::Utility::unthunk($convs->{$name}->{window}->map(0));
	}
	Warsaw::Utility::unthunk($window->map(0));
	exit(0);
    };
    $quit_b->action($quit_command);

    return {
	status => $buf,
	roster => $roster,
	log_in_out => $login_buf,  # contains text of login/logout button
    };
}

sub update_roster($$) {
    my ($roster, $main) = @_;
}

sub chat_starter_window() {
# Title
    my $title_text = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Babel $version"));
    $title_text = $kits->{ToolKit}->rgb($title_text, 1, 1, 1);
    my $hfil = $kits->{LayoutKit}->hfil();
    my $title = $kits->{LayoutKit}->hbox();
    $title->append($title_text);
    $title->append($hfil);

# Entry line
    my $indent = $kits->{LayoutKit}->hglueFil(100);
    
    my $name_buf = $kits->{CommandKit}->text();
    $name_buf->insertString(Warsaw::Utility::asc2uni("(type name here)"));
    my $textview = $kits->{TextKit}->simpleViewer($name_buf);
    $textview = $kits->{ToolKit}->rgb($textview, 1, 1, 1);
    my $input = $kits->{ToolKit}->textInput($textview, $name_buf);

    # Courtesy Dept. of Overenthusiastic Buttons
    my $chat_label = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Chat!"));
    my $chat_label = $kits->{ToolKit}->rgb($chat_label, 1, 1, 1);
    my $chat_c = Warsaw::Utility::command {
	my $name = Warsaw::Utility::uni2asc($name_buf->value());
	print("chat with \"$name\" requested\n");
	init_chat($name);
    };
    my $chat_b = $kits->{WidgetKit}->button($chat_label, $chat_c);

# Overall
    my $entry_line = $kits->{LayoutKit}->hbox();
    $entry_line->append($indent);
    $entry_line->append($input);
    $entry_line->append($chat_b);
    
    my $vbox = $kits->{LayoutKit}->vbox();
    my $vglue = $kits->{LayoutKit}->vglueFil(100);
    $vbox->append($title);
    $vbox->append($vglue);
    $vbox->append($entry_line);

    my $group = $kits->{ToolKit}->group($kits->{LayoutKit}->margin($vbox, 50));
    $group->appendController($input);
    my $window = $kits->{DesktopKit}->shell($group);
}

# Makes sure that a conversation window for $name is open
sub init_chat($) {
    my ($name) = @_;
    $name =~ s|/.*$||;
    print "init_chat with $name\n";
    if (!defined($convs->{$name})) {
	print "need new conversation window\n";
	$convs->{$name} = new_conversation($name);
	print "got it\n";
    }
    print "checking mapped status\n";
    if (! $convs->{$name}->{window}->mapped()) {
	print "need to be mapped\n";
	# FIXME: blank $convs->{$name}->$body?
	Warsaw::Utility::unthunk($convs->{$name}->{window}->map(1));
    }
    print "init_chat'ed\n";
}

sub new_conversation($) {
    my ($name) = @_;

    print "new_conversation: $name\n";
    my $vbox = $kits->{LayoutKit}->vbox();

# Text display at top
    my $body = $kits->{LayoutKit}->vbox();
    my $decorated_body = $kits->{ToolKit}->rgb($body, 1, 1, 1);
    my $area = $kits->{LayoutKit}->fixedSize($decorated_body,
					     5000,
					     5000,);
#    my $pager = $kits->{WidgetKit}->scrollable($decorated_text);
#    my $decorated_pager = $kits->{ToolKit}->rgb($pager, 0, 0, 0);
#    my $area = $kits->{LayoutKit}->fixedSize($decorated_pager,
#					     2000,
#					     2000,);
    my $pager = $kits->{WidgetKit}->scrollable($kits->{ToolKit}->rgb($area,
								     0,
								     0,
								     0,));
    my $fixed_pager = $kits->{LayoutKit}->fixedSize($pager,
						    2000,
						    2000,);
    $vbox->append($fixed_pager);

    print "1 ";
    
# entry window
    my $buf = $kits->{CommandKit}->text();
    $buf->insertString(Warsaw::Utility::asc2uni("enter message here"));
    my $entry = $kits->{TextKit}->simpleViewer($buf);
    my $decorated_entry = $kits->{ToolKit}->rgb($entry, 1, 1, 1);
#      my $spec = ToolKit::FrameSpec->new(ToolKit::outset);
#      $spec->bbrightness(0.5);
#      my $entry_frame = $kits->{ToolKit}->frame($decorated_entry,
#    					      10,
#    					      $spec,
#    					      1);
    my $entry_margin = $kits->{LayoutKit}->margin($decorated_entry, 40);
#    my $decorated_entry_margin = $kits->{ToolKit}->rgb($entry_margin, 0, 0, 0);
    my $editable = $kits->{ToolKit}->textInput($entry_margin, $buf);
    $vbox->append($editable);

    print "2 ";
    
# buttons along bottom
    my $buttons = $kits->{LayoutKit}->hbox();

    # "Send" button
    my $send_label = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Send"));
    $send_label = $kits->{LayoutKit}->margin($send_label, 20);
    $send_label = $kits->{ToolKit}->rgb($send_label, 0, 0, 0);

    print "3 ";
    
    my $send_command = Warsaw::Utility::command {
	my $text = Warsaw::Utility::uni2asc($buf->value());
	$buf->position(0);
	$buf->removeForward($buf->size());
	print "User entered \"$text\" for $name\n";
	my $line = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("$uid: $text"));
	$body->append($line);
	$line->needRedraw();
	$con->MessageSend(to => $name, body => $text);
    };
    my $send = $kits->{WidgetKit}->button($send_label,
					  $send_command);

    # "Close" button
    my $close_label = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("Close"));
    $close_label = $kits->{LayoutKit}->margin($close_label, 20);
    $close_label = $kits->{ToolKit}->rgb($close_label, 0, 0, 0);
    my $close = $kits->{WidgetKit}->button($close_label, undef);

    print "4 ";
    
    my $hfil = $kits->{LayoutKit}->hfil();
    $buttons->append($hfil);
    $buttons->append($send);
    $buttons->append($hfil);
    $buttons->append($close);
    $buttons->append($hfil);
    $vbox->append($buttons);

    my $margin = $kits->{LayoutKit}->margin($vbox, 200);
    my $group = $kits->{ToolKit}->group($margin);
    $group->appendController($pager);
    $group->appendController($editable);
    $group->appendController($send);
    $group->appendController($close);
    my $window = $kits->{DesktopKit}->shell($group);

    print "5 ";
    
    $close->action($window->map(0));

    print "6 ";
    return { window => $window, body => $body, entry => $buf };
}

sub jab_connect($$$$) {
    my ($uid, $host, $resource, $pass) = @_;
    
    if (!defined($pass)) {
	return "Error: bad call to jab_connect()";
    }

    $con = Net::Jabber::Client->new();
    $con->Connect( name => $host );
    $con->SetCallBacks(message => \&message_handler,
		       presence => \&presence_handler,
		       iq => \&iq_handler, );
    my @err = $con->AuthSend(username => $uid, password => $pass, resource => $resource);
    (@err[0] eq "ok") or return @err[1];
    
    my $pres = Net::Jabber::Presence->new();
    $con->Send($pres);

    return 0;
}

sub main_loop() {
    print "Entering main loop\n";
    while (1) {
	$con->Process();
	print "Looping\n";
    }
}

sub message_handler {
    my $m = new Net::Jabber::Message(@_);

    if ($m->GetType() eq "error") {
	print "got an error message ", $m->GetXML, "\n";
	return;
    }
    
    print("Received message from ", $m->GetFrom(), " saying ",
	  $m->GetBody(), "\n");
    my $name = $m->GetFrom();
    $name =~ s|/.*$||;
    $name =~ m/^(.*)\@/;
    my $uid = $1;
    print "(uid is $uid)\n";
    my $body = $m->GetBody();

    init_chat($name);
    
    my $line = $kits->{TextKit}->chunk(Warsaw::Utility::asc2uni("$uid:$body"));
    $convs->{$name}->{body}->append($line);
    $line->needRedraw();
}

use Data::Dumper;
sub presence_handler {
    print "Received presence:\n";
    print Dumper(@_);
}

sub iq_handler {
    print "Received iq:\n";
    print Dumper(@_);
}
