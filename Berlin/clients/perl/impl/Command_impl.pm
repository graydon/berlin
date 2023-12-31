# Automatically generated sample implementation code
# PLEASE EDIT     PLEASE EDIT      PLEASE EDIT.
# Generated on Sat Apr  1 04:01:31 2000 by idl2perl 2.4 with command:
# /usr/bin/idl2perl -out impls -impl Warsaw.idl

#use Command_types;
use Command_skel;

# interface Command (IDL:Command:1.0)

package Command_impl;
use COPE::CORBA::Servant;
@Command_impl::ISA=qw(CORBA::BOA::_Servant);
sub _skelname($) { 'Command_skel' }

# operation execute (IDL:Command/execute:1.0)

sub execute ($$) {
    my($self,$a) = @_;
}


1;
