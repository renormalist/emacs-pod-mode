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

    my $zil_weaver = first { $_->isa('Dist::Zilla::Plugin::PodWeaver') } @{ $zil->plugins };
    return unless $zil_weaver;
    my $weaver = $zil_weaver->weaver;
    my @weaver_plugins = @{ $weaver->plugins } ;

    print q{'((collectors . };
    emit_collectors(grep { $_->isa('Pod::Weaver::Section::Collect') } @weaver_plugins);
    print q{) (transformers . };
    emit_transformers(grep { $_->isa('Pod::Weaver::Plugin::Transformer') } @weaver_plugins);
    print qq{))\n};
}

sub emit_collectors {
    my @collectors = @_;
    print q{(};
    print join q{ } => map {
        sprintf q{(%s . %s)}, $_->command, $_->new_command;
    } @collectors;
    print q{)};
}

sub emit_transformers {
    my @transformers = @_;
    print q{(};
    emit_transformer_list(grep { $_->transformer->isa('Pod::Elemental::Transformer::List') } @transformers);
    print q{)};
}

sub emit_transformer_list {
    my @lists = @_;
    print join q{ } => map {
        sprintf q{(%s . List)}, $_->transformer->format_name;
    } @lists;
}

1;
