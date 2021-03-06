#!/usr/bin/perl

# Copyright (C) 2007-2018 X2Go Project - https://wiki.x2go.org
# Copyright (C) 2007-2018 Oleksandr Shneyder <o.shneyder@phoca-gmbh.de>
# Copyright (C) 2007-2018 Heinz-Markus Graesing <heinz-m.graesing@obviously-nice.de>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the
# Free Software Foundation, Inc.,
# 51 Franklin St, Fifth Floor, Boston, MA 02110-1301, USA.

package X2Go::Config;

=head1 NAME

X2Go::Config - X2Go Config package for Perl

=head1 DESCRIPTION

X2Go::Config Perl package for X2Go components.

=cut

use strict;
use Config::Simple;

use base 'Exporter';
our @EXPORT = ( 'get_config', 'get_sqlconfig', );

my $Config;
my $SqlConfig;

sub get_config {
	if (! defined $Config) {
		$Config = new Config::Simple(syntax=>'ini');
		$Config->read('/etc/x2go/x2goserver.conf' );
	}
	return $Config;
}

sub get_sqlconfig {
	if (! defined $SqlConfig) {
		$SqlConfig = new Config::Simple(syntax=>'ini');
		$SqlConfig->read('/etc/x2go/x2gosql/sql' );
	}
	return $SqlConfig;
}

1;
