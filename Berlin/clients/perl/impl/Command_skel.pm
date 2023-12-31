# Automatically generated skeleton code.  DO NOT EDIT.
# Generated on Sat Apr  1 04:01:31 2000 by idl2perl 2.4 with command:
# /usr/bin/idl2perl -out impls -impl Warsaw.idl

use COPE::CORBA::Skel;
#use Command_types;

# interface Command (IDL:Command:1.0)

package Command_skel;
use base qw(CORBA::_Skel);
$Command_skel::_id = 0;

$Command_skel::_interface = 'IDL:Command:1.0';

sub new {
    my $class = shift;
    my $id = shift || $Command_skel::_id++;
    my $self = bless [\%Command_skel::FIELDS, $id], $class;
    return $self;
}

# operation execute (IDL:Command/execute:1.0)

sub execute ($$) {
    my($self,$serverrequest) = @_;
    my $arg_list = [
          { 'argument'  =>
            { _type  => $CORBA::_tc_any },
            'arg_modes' => 0,
          },
    ];
    $serverrequest->params($arg_list);
    $self->{impl}->execute(
        $arg_list->[0]{argument}{_value},
    );
}

1;
