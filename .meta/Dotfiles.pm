package Dotfiles;

use Exporter 'import';

use File::Basename 'fileparse';
use Cwd 'realpath';
use Term::ANSIColor qw(:constants);


my @EXPORTED = qw(
    $HOME
    dir
    ex
    gprint
);

@EXPORT_OK = @EXPORTED;
%EXPORT_TAGS = (ALL => [@EXPORTED]);


$HOME = $ENV{"HOME"};

sub dir {
    my ($_filename, $dirname, $_suffix) = fileparse(realpath(shift));
    return $dirname;
}

sub gprint {
    print GREEN, @_, RESET, "\n";
}

sub ex {
    system(@_) == 0
	or die RED, @_, " FAILED", RESET;
}
