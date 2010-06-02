use CPAN::Meta;

my $VERSION = `grep '^;;; Version:' pod-mode.el | cut -d: -f2 | sed 's/ //g'`;
chomp $VERSION;

my $distmeta = {
                abstract       => "Emacs syntax highlighting for POD",
                description    => "Emacs syntax highlighting for POD",
                dynamic_config => "0",
                generated_by   => "An inappropriate amount of Emacs and Perl",
                name           => "pod-mode",
                release_status => "stable",
                version        => "$VERSION",
                author         => [
                                   'Steffen Schwigon <ss5@renormalist.net>',
                                   'Florian Ragwitz <rafl@debian.org>',
                                  ],
                keywords       => [ "emacs", "mode", "perl", "pod" ],
                license        => [ "perl_5" ],
                resources      => {
                                   bugtracker => {
                                                  mailto => 'bug-pod-mode@rt.cpan.org',
                                                  web    => "http://rt.cpan.org/Public/Dist/Display.html?Name=pod-mode"
                                                 },
                                   homepage => "http://search.cpan.org/dist/pod-mode",
                                   repository => {
                                                  type => "git",
                                                  url  => "git://github.com/renormalist/emacs-pod-mode.git",
                                                  web  => "http://github.com/renormalist/emacs-pod-mode",
                                                 },
                                  },
               };

my $cpanmeta = CPAN::Meta->create($distmeta)->save("META.json");
