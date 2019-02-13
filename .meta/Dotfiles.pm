package Dotfiles;

use Exporter 'import';

use File::Basename 'dirname';
use Cwd 'realpath';
use Term::ANSIColor qw(:constants);


my @EXPORTED = qw(
    $HOME
    dir
    gprint
);

@EXPORT_OK = @EXPORTED;
%EXPORT_TAGS = (ALL => [@EXPORTED]);


$HOME = $ENV{"HOME"};

sub dir {
    return dirname(realpath(__FILE__));
}

sub gprint {
    print GREEN, @_, RESET, "\n";
}
