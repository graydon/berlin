package Block;
use strict;

sub new {
  my $self = {};

  $self->{_START} = hex($_[1]);
  $self->{_END}   = hex($_[2]);
  $self->{_NAME}  = $_[3];

  bless($self);
  return $self;
}


sub start {
  my $self = shift;
  return $self->{_START};
}

sub start_string {
  my $self = shift;
  return sprintf "0x%X", $self->{_START};
}

sub end {
  my $self = shift;
  return $self->{_END};
}

sub end_string {
  my $self = shift;
  return sprintf "0x%X", $self->{_END};
}

sub name {
  my $self = shift;
  return $self->{_NAME};
}

sub filename {
  my $self = shift;

  my $tmp = join '_', split / /, $self->{_NAME};

  return sprintf "%X-%X.cc", $self->start(), , $self->end();
}

sub classname {
  my $self = shift;

  my $tmp = join '', split /-/, join '_', split / /, $self->{_NAME};
  $tmp = sprintf "%s%X", $tmp, $self->{_START};
  return $tmp;
}

1; # for use to succeed...
