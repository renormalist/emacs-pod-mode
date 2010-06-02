use CPAN::Meta;

my $distmeta = {
                abstract       => "Emacs syntax highlighting for POD",
                description    => "Emacs syntax highlighting for POD",
                dynamic_config => "0",
                generated_by   => "An inappropriate amount of Emacs and Perl",
                name           => "pod-mode",
                release_status => "stable",
                version        => "1.00",
                author         => [
                                   'Steffen Schwigon <ss5@renormalist.net>',
                                   'Florian Ragwitz <rafl@debian.org>',
                                  ],
                keywords       => [ "emacs", "mode", "perl", "pod" ],
                license        => [ "perl_5" ],
               };

my $cpanmeta = CPAN::Meta->create($distmeta)->save("META.json");
