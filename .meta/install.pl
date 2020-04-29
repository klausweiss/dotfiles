#!/usr/bin/env perl
use strict;
use warnings;

use Cwd 'realpath';
use Env qw(HOME);
use File::Basename 'fileparse';
use File::Path 'make_path';
use File::Spec;
use Term::ANSIColor qw(:constants);

use Class::Struct InstallConfig => {
    commands => '@',
    links => '@',
};

use Class::Struct Command => {
    command => '$',
    comment => '$',
    workdir => '$',
};

use Class::Struct Link => {
    src => '$',
    target => '$',
};

sub main {
    if (is_executing()) {
        my $executed_file = get_executed_file_path();

        my $config = parse_config_from_file($executed_file);

        my $links = $config->links;
        create_links(@$links);

        my $commands = $config->commands;
        run_commands(@$commands);
    }
}

sub is_executing {
    return
        ($#ARGV >= 0) &&
        ($ARGV[0] eq "--exec");
}

sub get_executed_file_path {
    return $ARGV[1];
}

sub parse_config_from_file {
    my $file_path = shift;
    my $workdir = containing_dir($file_path);
    
    my $config = new InstallConfig();

    open(my $file, "<", $file_path);
    parse($workdir, $config, $file);
    close $file;
    
    return $config;
}

sub parse {
    my ($workdir, $config, $file) = @_;
    my @buffer = ();

    my $links = $config->links;

    *do_parse = sub {
        skip_headers();
        while (parse_config_block()) {}
    };

    *process_lines = sub {
        my $process_line = shift;
        
        while (scalar @buffer > 0) {
            my $line = shift(@buffer);

            if (&$process_line($line) eq -1) {
                return -1;
            }
        }
        
        while (my $line = <$file>) {
            if (&$process_line($line) eq -1) {
                return -1;
            }
        }
    };

    *for_every_line = sub {
        my $lines_processor = shift;
        return sub {
            return process_lines(\&$lines_processor);
        };
    };

    *skip_headers = for_every_line(
        sub {
            my $line = shift;
            if (starts_section($line)) {
                push(@buffer, $line);
                return -1;
            }
        });

    *starts_section = sub {
        return shift =~ /\[.*\]/;
    };

    *is_comment = sub {
        return shift =~ /^\s*#/;
    };

    *trim = sub {
        for my $str (@_) {
            $str =~ s/^\s+|\s+$//g;
        }
    };

    *get_section = sub {
        my ($words) = (shift =~ m/\[\s*(.*)?\s*\]/);
        return $words;
    };

    my %sections_parsers;
    
    *parse_config_block = sub {
        my $section = "none";
        *parse_section = sub {
            my $line = shift;
            $section = get_section($line);
            return -1
        };

        process_lines(\&parse_section);
        undef &parse_section;

        my $parser = $sections_parsers{$section};
        if (!defined $parser) {
            die "unknown section: ", $section;
        }
        &$parser();
    };

    *parse_symlink_block = for_every_line(
        sub {
            my $line = shift;
            if (starts_section($line)) {
                push(@buffer, $line);
                return -1;
            }
            if ($line =~ /\S/) {
                parse_symlink_line($line);
            }
        });
    
    *parse_symlink_line = sub {
        my $line = shift;
        my ($src, $target) = ($line =~ m/(.*): (.*)/);
        trim($src, $target);
	my $link = new Link(
	    src => $src,
	    target => $target,
	    );
	push(@$links, $link);
    };

    *parse_run_block = sub {
        my $current_command;
        my $commands = $config->commands;
        *parse_run_line = sub {
            my $line = shift;
            trim($line);
            if (starts_section($line)) {
                push(@buffer, $line);
                return -1;
            }
            
            my $is_comment = is_comment($line);
            if (!defined $current_command || $is_comment) {
                $current_command = new Command(
                    workdir => $workdir,
                    );
            }
            if ($is_comment) {
                $current_command->comment($line);
            } elsif (!defined $current_command->command) {
                $current_command->command($line);
                push(@$commands, $current_command);
            }
        };
        my $result = process_lines(\&parse_run_line);
        undef &parse_run_line;
        return $result;
    };
    
    %sections_parsers = (
        "symlink" => \&parse_symlink_block,
        "run" => \&parse_run_block,
        );
    
    *relative_to_absolute_paths = sub {
	foreach my $link (@$links) {
            $link->src(absolute_path($link->src));
            $link->target(absolute_path($link->target));
        }
    };

    *absolute_path = sub {
        my $path = shift;
        $path =~ s/^~/$HOME/;
        return File::Spec->rel2abs($path, $workdir);
    };
    
    do_parse();
    relative_to_absolute_paths();
}

sub create_links {
    my @links = @_;

    foreach my $link (@links) {
        mk_symlink($link);
    }
}

sub run_commands {
    my @commands = @_;

    foreach my $command (@commands) {
        if (defined $command->comment) {
            print_comment($command->comment);
        }
        run_command(
            $command->workdir,
            $command->command,
            );
    }
}

sub containing_dir {
    my ($_filename, $dirname, $_suffix) = fileparse(shift);
    return substr($dirname, 0, -1);
}

sub mk_symlink {
    my $link = shift;
    print
        GREEN, "Linking ",
        MAGENTA, $link->src,
        GREEN, " to ",
        BLUE, $link->target,
        RESET, "\n";

    unless (-e $link->src) {
        die RED "File ", UNDERSCORE $link->src, CLEAR RED " does not exist", RESET;
    }

    my $target_dir = containing_dir($link->target);
    make_path($target_dir);
    symlink($link->src, $link->target);
}

sub print_comment {
    my $comment = shift;
    $comment =~ s/^#\s*//;
    print
        BOLD GREEN, $comment,
        RESET, "\n";
}

sub run_command {
    my ($workdir, $command) = @_;
    system("(cd $workdir; $command)");
}

main();
