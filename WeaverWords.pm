use strict;
use warnings;

package WeaverWords;

use Sub::Exporter -setup => {
    exports => ['weaverwords_from_dir'],
    groups  => { default => ['weaverwords_from_dir'] },
};

use Dist::Zilla;
use Dist::Zilla::Chrome::Test;
use List::AllUtils qw(first);

sub weaverwords_from_dir {
    my ($dir) = @_;

    my $zil = Dist::Zilla->from_config({
        chrome    => Dist::Zilla::Chrome::Test->new,
        dist_root => $dir,
    });

    my $zil_weaver = first {
        $_->isa('Dist::Zilla::Plugin::PodWeaver')
    } @{ $zil->plugins};
    return unless $zil_weaver;

    my @weaver_plugins = @{ $zil_weaver->weaver->plugins };

    print SExpGen->new->visit({
        collectors => [
            map {
                +{ $_->command, $_->new_command }
            } grep {
                $_->isa('Pod::Weaver::Section::Collect')
            } @weaver_plugins
        ],
        transformers => [
            map {
                $_->transformer->isa('Pod::Elemental::Transformer::List')
                    ? +{ 'List' => $_->transformer->format_name }
                    : ()
            } grep {
                $_->isa('Pod::Weaver::Plugin::Transformer')
            } @weaver_plugins
        ],
    }), "\n";
}

package SExpGen;
# perl data -> sexp. only simple values, hash refs, and array refs.

use Moose;
use List::AllUtils qw(reduce);
use namespace::autoclean;

extends 'Data::Visitor';

sub visit_value {
    my ($self, $value) = @_;
    return qq{(quote $value)};
}

override visit_normal_hash => sub {
    my ($self) = @_;
    my $ret = super;
    return $self->foldr_cons(map {
        sprintf q{(cons %s %s)}, $_, $ret->{$_}
    } keys %{ $ret });
};

override visit_normal_array => sub {
    my ($self) = @_;
    return $self->foldr_cons(@{ super() }, q{nil});
};

sub foldr_cons {
    my ($self, @list) = @_;

    return join q{ } => reduce {
        sprintf q{(cons %s %s)}, $b, $a;
    } reverse @list;
}

1;
