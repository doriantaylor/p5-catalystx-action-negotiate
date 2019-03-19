package TestApp::Controller::Root;
use Moose;
use namespace::autoclean;

BEGIN { extends 'Catalyst::Controller' }

#
# Sets the actions in this controller to be registered with no prefix
# so they function identically to actions created in MyApp.pm
#
__PACKAGE__->config(namespace => '');

=encoding utf-8

=head1 NAME

TestApp::Controller::Root - Root Controller for TestApp

=head1 DESCRIPTION

[enter your description here]

=head1 METHODS

=head2 index

The root page (/)

=cut

sub index :Path :Args(0) {
    my ( $self, $c ) = @_;

    # Hello World
    $c->response->body( $c->welcome_message );
}

# sub conneg :Local :ActionClass('+CatalystX::Action::Negotiate') {
#     my ($self, $c) = @_;

#     my @variants = @{$c->stash->{variants} || []};

#     $c->log->debug(length @variants);
#     $c->log->debug($c->res->status);

#     # if (length @variants) {
#     #     $c->res->status(200);
#     #     #$c->res->body(length @variants);
#     # }
#     # else {
#     #     $c->res->body('fail');
#     # }
# }

=head2 default

Standard 404 error page

=cut

sub default :Path :ActionClass('+CatalystX::Action::Negotiate') {
    my ( $self, $c ) = @_;

    $c->response->body( 'Page not found' );
    $c->response->status(404);
}

=head2 end

Attempt to render a view, if needed.

=cut

#sub end : ActionClass('RenderView') {}

=head1 AUTHOR

dorian,,,

=head1 LICENSE

This library is free software. You can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

__PACKAGE__->meta->make_immutable;

1;
