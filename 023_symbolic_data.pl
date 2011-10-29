#!/usr/bin/env perl
use strict; use warnings;

# Sets as binary trees

# Basic implementation
sub entry { my $tree = shift; return ($tree or [])->[0]; }
sub left_branch { my $tree = shift; return ($tree or [])->[1]; }
sub right_branch { my $tree = shift; return ($tree or [])->[2]; }
sub make_tree { my ($entry, $left, $right) = @_; return [$entry, $left, $right]; }

sub list_to_tree {
    my $elements = shift;

    return _partial_tree($elements, scalar @{$elements or []})->[0];
}

sub _partial_tree {
    my ($elements, $n) = @_;

    my $out;
    if ($n != 0) {
        my $left_size = int(($n - 1) / 2);
        my $left_result = _partial_tree($elements, $left_size);

        my ($left_tree, $non_left_elements) = @{$left_result};

        my $root_entry = pop @{$non_left_elements};

        my $right_size = ($n - ($left_size + 1));
        my $right_result = _partial_tree($non_left_elements, $right_size);

        my ($right_tree, $remaining_elements) = @{$right_result};

        $out = [
            make_tree($root_entry, $left_tree, $right_tree),
            $remaining_elements
        ];
    }
    else {
        $out = [[], $elements];
    }

    return $out;
}

my $tree = list_to_tree([qw/ 1 2 3 4 5 6 7 /]);
use Data::Dumper; warn Dumper $tree;

# vim:fdm=marker
