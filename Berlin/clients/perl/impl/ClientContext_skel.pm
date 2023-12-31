# Automatically generated skeleton code.  DO NOT EDIT.
# Generated on Wed Jan 26 17:55:37 2000 by idl2perl 1.22 with command:
# /usr/bin/idl2perl -impl out.idl

use COPE::CORBA::Skel;
#use ClientContext_types;
# interface ClientContext (IDL:ClientContext:1.0)

package ClientContext_skel;
use base qw(CORBA::_Skel);
$ClientContext_skel::_id = 0;

$ClientContext_skel::_interface = 'IDL:ClientContext:1.0';

sub new {
    my $class = shift;
    my $id = shift || $ClientContext_skel::_id++;
    my $self = bless [\%ClientContext_skel::FIELDS, $id], $class;
    return $self;
}

# attribute userName (IDL:ClientContext/userName:1.0)

sub userName ($$) {
    my($self,$serverrequest) = @_;
    my $result_ = { _type => $Unistring::_tc };
    $serverrequest->params([]);
    $serverrequest->result($result_);
    $result_->{_value} = $self->{impl}->userName();
}

# operation ping (IDL:ClientContext/ping:1.0)

sub ping ($$) {
    my($self,$serverrequest) = @_;
    my $arg_list = [
    ];
    $serverrequest->params($arg_list);
    $self->{impl}->ping(
    );
}


1;
