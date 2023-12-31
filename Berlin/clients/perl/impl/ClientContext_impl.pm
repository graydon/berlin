# Automatically generated sample implementation code
# PLEASE EDIT     PLEASE EDIT      PLEASE EDIT.
# Generated on Wed Jan 26 17:55:37 2000 by idl2perl 1.22 with command:
# /usr/bin/idl2perl -impl out.idl

#use ClientContext_types;
use ClientContext_skel;

# interface ClientContext (IDL:ClientContext:1.0)

package ClientContext_impl;
use COPE::CORBA::Servant;
@ClientContext_impl::ISA=qw(CORBA::BOA::_Servant);
sub _skelname($) { 'ClientContext_skel' }

sub new {
    my($class,@args) = @_;
    my $self = {@args};
    bless $self, $class;
    $CORBA::BOA::_The_Boa->activate_object($self);
    return $self;
}

# attribute userName (IDL:ClientContext/userName:1.0)

sub userName ($) {
    my($self) = @_;
#    return $self->{'userName'};
    return [];
}

# operation ping (IDL:ClientContext/ping:1.0)

sub ping ($) {
    my($self) = @_;
#    print "Pinged\n";
}


1;
