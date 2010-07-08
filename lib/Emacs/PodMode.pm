package Emacs::PodMode; 
# ABSTRACT: Emacs major mode for editing .pod-files

1;

=head1 DESRIPTION

POD is the Plain Old Documentation format of Perl. This mode supports
writing POD.

=head1 USAGE

Put the file F<pod-mode.el> into your load-path and the following into
your F<~/.emacs>:

    (require 'pod-mode)

To associate pod-mode with .pod files add the following to your
F<~/.emacs>:

    (add-to-list 'auto-mode-alist '("\\.pod$" . pod-mode))

To automatically turn on font-lock-mode add the following to your
F<~/.emacs>:

    (add-hook 'pod-mode-hook 'font-lock-mode)

In addition to the standard POD commands, custom commands as defined
by a L<Pod::Weaver> configuration are supported. However, for those to
work, F<eproject.el> as available at
L<http://github.com/jrockway/eproject> is required.

Make sure to require F<eproject.el> or create an autoload for
C<eproject-maybe-turn-on> if you expect custom commands to work.

When automatically inserting hyperlink formatting codes to modules or
sections within modules, autocompletion for module names will be
provided if perldoc.el, as available at
L<git://gaffer.ptitcanardnoir.org/perldoc-el.git>, is present.

=head1 SEE ALSO

For the actual mode please refer to F<pod-mode.el>.

