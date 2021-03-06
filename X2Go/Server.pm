#!/usr/bin/perl

# Copyright (C) 2007-2018 X2Go Project - https://wiki.x2go.org
# Copyright (C) 2007-2018 Oleksandr Shneyder <o.shneyder@phoca-gmbh.de>
# Copyright (C) 2007-2018 Heinz-Markus Graesing <heinz-m.graesing@obviously-nice.de>
# Copyright (C) 2010-2015 Mike Gabriel <mike.gabriel@das-netzwerkteam.de>
# Copyright (C) 2013-2015 Guangzhou Nianguan Electronics Technology Co.Ltd. <opensource@gznianguan.com>
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

package X2Go::Server;

=head1 NAME

X2Go::Server - X2Go Server package for Perl

=head1 DESCRIPTION

X2Go::Server Perl package.

=cut

use strict;
use Sys::Hostname;
use X2Go::Server::DB qw(db_listsessions);

use base 'Exporter';
our @EXPORT = ( 'get_session_info', );

sub get_session_info {
	my $theX2GoSID = $_[0];
	foreach  my $sessionLine (db_listsessions(hostname)) {
		if ($sessionLine =~ /$theX2GoSID/) {
			return split(/\|/,$sessionLine);
		}
	}
	return 0;
}

1;
