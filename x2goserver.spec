Name:           x2goserver
Version:        4.1.0.4
Release:        0.0x2go1%{?dist}
Summary:        X2Go Server

%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
License:        GPLv2+
%else
Group:          Productivity/Networking/Remote Desktop
License:        GPL-2.0+
%endif

URL:            https://www.x2go.org
Source0:        https://code.x2go.org/releases/source/%{name}/%{name}-%{version}.tar.gz
Source1:        %{name}.service
Source2:        %{name}.init
Source3:        %{name}-rpmlintrc

%if 0%{?el5}
# For compatibility with EPEL5
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
%else
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
%endif

# Fedora 29+ doesn't install gcc and g++ automatically any longer,
# but this dependency should be useful for all distros really.
BuildRequires:  gcc
%if 0%{?fedora} > 28
# The annobin plugin is enabled by default and redhat-rpm-config depends
# on it... if the gcc package is installed.
# Since Fedora 29+ removed gcc as a common package pulled in when
# generating the build chroot, redhat-rpm-config is not pulling annobin
# in.
# If we later install the gcc package, the rich/boolean dependency within
# redhat-rpm-config doesn't seem to be re-evaluated, so we're left with a
# mess.
# Explicitly depend upon annobin, at least for now.
BuildRequires:  annobin
%endif

BuildRequires:  findutils

# We need this package for SuSE-specific macros.
# RHEL-based systems should be fine without them, but
# further checking with rpmlint is required.
%if 0%{?suse_version}
BuildRequires:  shared-mime-info
%endif

# Always needed, on all platforms, for desktop-file-validate.
BuildRequires:  desktop-file-utils

%if 0%{?fedora} || 0%{?rhel}
BuildRequires:  perl-generators
%endif
BuildRequires:  perl(ExtUtils::MakeMaker)

%if 0%{?fedora} || 0%{?rhel} >= 7
BuildRequires:  man2html-core
%else
BuildRequires:  man
%endif

%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
BuildRequires:  systemd
%if 0%{?suse_version} >= 1210
BuildRequires:  systemd-rpm-macros
%endif
%endif

# So XSESSIONDIR gets linked
%if 0%{?suse_version}
%if 0%{?suse_version} <= 1130
BuildRequires: xorg-x11
%else
BuildRequires: xinit
%endif
%else
BuildRequires:  xorg-x11-xinit
%endif

# Packages only needed for file system paths.
BuildRequires:  sudo
%if 0%{?fedora} > 19 || 0%{?el5} || 0%{?el6} || 0%{?el7}
BuildRequires:  logcheck
%endif

# for %%{_libdir}/nx/bin to be owned by some package
BuildRequires:  nxagent

%if 0%{?suse_version}
Requires:       openssh
%else
Requires:       openssh-server
%endif

%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%endif

# For running any command, really.
Requires:       bash
# For x2goruncommand - for now
Requires:       bc
# For x2goshowblocks
Requires:       lsof
# For netstat in x2goresume-session
Requires:       net-tools
Requires:       perl(Try::Tiny)
# We need a database
# For killall in x2gosuspend-session
Requires:       psmisc
# For x2godbadmin
Requires:       pwgen
# For printing, file-sharing
Requires:       sshfs
# For /etc/sudoers.d
Requires:       sudo
Requires:       x2goserver-x2goagent = %{version}-%{release}
Requires(post): grep
Requires:       grep
Requires:       perl(File::Which)
Requires:       perl(File::BaseDir)

Requires:       perl(Config::Simple)
Requires:       perl(Switch)

Requires:       xkeyboard-config

%if 0%{?suse_version}
%if 0%{?suse_version} >= 1220
Requires:       setxkbmap xmessage xwininfo
%else
Requires:       xorg-x11
%endif
%else
Requires:       xorg-x11-fonts-misc
Requires:       xorg-x11-xauth
Requires:       which
%endif

%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
%{?systemd_requires}
%endif

Requires:       perl-X2Go-Server = %{version}-%{release}
Requires(post): perl-X2Go-Server-DB
Requires(post): x2goserver-common
Requires:       x2goserver-common
Requires:       x2goserver-extensions
Requires:       x2goserver-xsession
%if 0%{?fedora} >= 21 || 0%{?suse_version} >= 1110
Suggests:     x2goserver-fmbindings
Suggests:     x2goserver-printing
%endif

%{?perl_default_filter}

%description
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains the main daemon and tools for X2Go server-side session
administrations.


%package common
Summary:        X2Go Server (common files)
%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%endif

# for useradd/groupadd
# OpenSUSE 12.3 and SLE{S,D} 12 (has suse_version 1315) switched to shadow.
%if 0%{?suse_version} >= 1230
BuildRequires:  shadow
Requires(pre):  shadow
%else
# Older SuSE versions (prior to 12.3) only have pwdutils.
%if 0%{?suse_version}
BuildRequires:  pwdutils
Requires(pre):  pwdutils
# Fedora, EPEL and RedHat use shadow-utils.
%else
BuildRequires:  shadow-utils
Requires(pre):  shadow-utils
%endif
%endif

%if 0%{?suse_version} > 1500
# OpenSuSE Tumbleweed currently has a bug and doesn't pull in
# openSUSE-release automatically, leaving /etc/os-release a dangling
# symlink.
# Why? No idea.
BuildRequires:  openSUSE-release
%endif

%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description common
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains common files needed by the X2Go Server
and the X2Go::Server Perl API.


%package -n perl-X2Go-Server
Summary:        Perl X2Go::Server package
Requires:       x2goserver-common = %{version}-%{release}
Requires:       perl-X2Go-Log = %{version}-%{release}
Requires:       perl-X2Go-Server-DB = %{version}-%{release}
%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%else
Requires:       perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))
%endif
Requires:       perl(Capture::Tiny)
%if 0%{?fedora} || 0%{?rhel}
Group:          Development/Libraries
%else
Group:           Development/Libraries/Perl
%endif

%description -n perl-X2Go-Server
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains the X2Go::Server Perl package.


%package -n perl-X2Go-Server-DB
Summary:        Perl X2Go::Server::DB package
Requires:       x2goserver-common = %{version}-%{release}
Requires:       perl-X2Go-Log = %{version}-%{release}
Requires:       perl(Config::Simple)
Requires(post): perl(DBD::SQLite)
Requires:       perl(DBD::SQLite)
Requires:       perl(DBD::Pg)
#Requires:       perl(DBD::mysql)
%if 0%{?suse_version}
Requires(pre):  permissions
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%else
Requires:       perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))
%endif
%if 0%{?fedora} || 0%{?rhel}
Group:          Development/Libraries
%else
Group:          Development/Libraries/Perl
%endif

%description -n perl-X2Go-Server-DB
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains the X2Go::Server::DB Perl package.

%package -n perl-X2Go-Log
Summary:        Perl X2Go::Log package
Requires:       x2goserver-common = %{version}-%{release}
%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%else
Requires:       perl(:MODULE_COMPAT_%(eval "`%{__perl} -V:version`"; echo $version))
%endif
%if 0%{?fedora} || 0%{?rhel}
Group:          Development/Libraries
%else
Group:          Development/Libraries/Perl
%endif

%description -n perl-X2Go-Log
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains the X2Go::Log Perl package.

%package x2goagent
Group:          Applications/System
Summary:        X2Go Server's X2Go Agent Xserver
Requires:       nxagent >= 3.5.99.17
Conflicts:      x2goagent < 3.5.99.0
Obsoletes:      x2goagent < 3.5.99.0

%description x2goagent
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

X2Go is a software suite that uses NX and/or KDrive technology for remote
desktop computing.

NX technology implements a very efficient compression of the X11
protocol. This increases performance when using X applications over a
network, especially a slow one.

X2Go agent functionality has been completely incorporated into NX
agent's code base. If the nxagent binary is executed under the name of
`x2goagent', the X2Go functionalities get activated.

This package is a wrapper that activates X2Go branding in nxagent.
Please refer to the nxagent package's description for more information
on NX.

%package x2gokdrive
Group:          Applications/System
Summary:        X2Go Server's X2Go KDrive Xserver
Requires:       xserver-x2gokdrive

%description x2gokdrive
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

X2Go is a software suite that uses NX and/or KDrive technology for
remote desktop computing.

X2Go KDrive technology implements a remote X11 Xserver backend for
modern desktop environments, namely desktops derived from the GNOME
desktop shell.

X2Go KDrive does not require an XServer on the client-side, only the
X11-independent x2gokdriveclient. esktop session data transfers from
server to client use differential image compression and image data gets
cached client-side.

%package printing
Summary:        X2Go Server (printing support)
Requires:       %{name} = %{version}-%{release}
%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%endif

# for useradd/groupadd
# OpenSUSE 12.3 and SLE{S,D} 12 (has suse_version 1315) switched to shadow.
%if 0%{?suse_version} >= 1230
BuildRequires:  shadow
Requires(pre):  shadow
%else
# Older SuSE versions (prior to 12.3) only have pwdutils.
%if 0%{?suse_version}
BuildRequires:  pwdutils
Requires(pre):  pwdutils
# Fedora, EPEL and RedHat use shadow-utils.
%else
BuildRequires:  shadow-utils
Requires(pre):  shadow-utils
%endif
%endif

%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description printing
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

The X2Go Server printing package provides client-side printing support for
X2Go.

This package has to be installed on X2Go servers that shall be able to pass
X2Go print jobs on to the X2Go client.

This package co-operates with the cups-x2go CUPS backend. If CUPS server and
X2Go server are hosted on different machines, then make sure you install
this package on the X2Go server(s) (and the cups-x2go package on the CUPS
server).


%package desktopsharing
Summary:        X2Go Server (Desktop Sharing support)
Requires:       %{name} = %{version}-%{release}
Requires:       x2godesktopsharing >= 3.2.0.0
%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description desktopsharing
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - audio support
    - authentication by smartcard and USB stick

X2Go Desktop Sharing is an X2Go add-on feature that allows a user to
grant other X2Go users access to the current session (shadow session
support). The user's current session may be an X2Go session itself or
simply a local X11 session.

This package contains all the integration and configuration logics
of a system-wide manageable desktop sharing setup.


%package extensions
Summary:        X2Go Server (extension support)
Requires:       %{name} = %{version}-%{release}
%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description extensions
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

The X2Go Server extension namespace offers contributors
to add script functionality to X2Go.

Make sure you have this package installed on your server
if you want X2Go clients to be able to access your server
without lack of features.


%package xsession
Summary:        X2Go Server (Xsession runner)
Requires:       %{name} = %{version}-%{release}
# For /etc/X11/Xresources
%if 0%{?suse_version}
%if 0%{?suse_version} <= 1130
Requires:       xorg-x11
%else
Requires:       xinit
%endif
%else
Requires:       xorg-x11-xinit
%endif

# dbus-run-session is tricky, so using a separate section for it.
# For *SuSE:
#   It's only available on SLE 12+ and OpenSuSE 13+.
#   With OpenSuSE Leap 42.3, it was migrated from dbus-1-x11 to dbus-1.
# For CentOS:
#   Not available at all.
# For Fedora:
#   Supported on all versions we care about as part of the dbus package.
%if 0%{?suse_version}
%if 0%{?suse_version} >= 1300
%if 0%{?sle_version} >= 120300 && 0%{?is_opensuse}
Requires:       dbus-1
%else
Requires:       dbus-1-x11
%endif
%endif
%endif
%if 0%{?fedora}
Requires:       dbus
%endif

%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%if 0%{?suse_version}
%if 0%{?suse_version} < 1140
Requires:       perl = %{perl_version}
%else
%{perl_requires}
%endif
%endif

Requires:       perl(Cwd)

%description xsession
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This X2Go Server add-on enables Xsession script handling
when starting desktop sessions with X2Go.

Amongst others the parsing of Xsession scripts will
enable desktop-profiles, ssh-agent startups, gpgagent
startups and many more Xsession related features on
X2Go session login automagically.


%package fmbindings
Summary:        X2Go Server (file manager bindings)
Requires:       %{name} = %{version}-%{release}
%if 0%{?suse_version}
%if 0%{?suse_version} <= 1130
Requires(pre):    shared-mime-info
%endif
Requires(post):   shared-mime-info
Requires(postun): shared-mime-info
%endif
Requires:       xdg-utils
%if 0%{?suse_version} || ( 0%{?fedora} && 0%{?fedora} < 25 ) || ( 0%{?rhel} && 0%{?rhel} < 8 )
Requires(post):   desktop-file-utils
Requires(postun): desktop-file-utils
%if ( 0%{?fedora} && 0%{?fedora} < 24 ) || ( 0%{?rhel} && 0%{?rhel} < 8 )
Requires(posttrans):  shared-mime-info
%endif
%endif
%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description fmbindings
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains generic MIME type information
for X2Go's local folder sharing. It can be used with all
freedesktop.org compliant desktop shells.

However, this package can be superseded by other, more specific
destkop binding components, if installed and being used with the
corresponding desktop shell:
    - under LXDE by x2golxdebindings
    - under GNOMEv2 by x2gognomebindings
    - under KDE4 by plasma-widget-x2go
    - under MATE by x2gomatebindings


%package logcheck
Summary:        X2Go Server (logcheck configuration)
Requires:       %{name} = %{version}-%{release}
%if 0%{?fedora} > 19 || 0%{?el5} || 0%{?el6} || 0%{?el7}
Requires:       logcheck
%endif

%if 0%{?fedora} || 0%{?rhel}
Group:          Applications/Communications
%else
Group:          Productivity/Networking/Remote Desktop
%endif

%description logcheck
X2Go is a server based computing environment with
    - session resuming
    - low bandwidth support
    - session brokerage support
    - client side mass storage mounting support
    - client side printing support
    - audio support
    - authentication by smartcard and USB stick

This package contains the logcheck configuration files that avoid
false-positives when running X2Go sessions.


%prep
%setup -q

# Don't try to be root
sed -i -e 's/-o root -g root//' */Makefile

%build
make %{?_smp_mflags} CFLAGS='%{?__global_cppflags} %{?__global_cflags} %{optflags}' LDFLAGS='%{?__global_ldflags}' PERL_INSTALLDIRS='vendor' PREFIX='%{_prefix}' NXLIBDIR='%{_libdir}/nx' LIBDIR='%{_libdir}/x2go'


%install
make install DESTDIR='%{buildroot}' PREFIX='%{_prefix}' NXLIBDIR='%{_libdir}/nx' LIBDIR='%{_libdir}/x2go'

# Make sure the .packlist file is removed from %%{perl_vendorarch}...
find %{buildroot}%{perl_vendorarch} -name .packlist | while read file; do rm -f "$file"; done

# Remove placeholder files (in a way that works on EPEL-5, as well)
find %{buildroot}%{_libdir}/x2go/extensions/ -type f -name ".placeholder" | while read file; do rm -f "$file"; done

# x2gouser homedir, state dir
mkdir -p %{buildroot}%{_localstatedir}/lib/x2go/
# Create empty session file for %%ghost
touch %{buildroot}%{_localstatedir}/lib/x2go/x2go_sessions

# Printing spool dir
mkdir -p %{buildroot}%{_localstatedir}/spool/x2goprint

%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
# System.d session cleanup script
mkdir -p %{buildroot}%{_unitdir}
install -pm0644 %SOURCE1 %{buildroot}%{_unitdir}
%else
# SysV session cleanup script
%if 0%{?el5}
mkdir -p %{buildroot}%{_initrddir}
install -pm0755 %SOURCE2 %{buildroot}%{_initrddir}/x2goserver
%else
mkdir -p %{buildroot}%{_initddir}
install -pm0755 %SOURCE2 %{buildroot}%{_initddir}/x2goserver
%endif
%if 0%{?suse_version} && 0%{?suse_version} < 1210
ln -sf %{_initddir}/x2goserver %{buildroot}%{_sbindir}/rcx2goserver
%endif
%endif

%if 0%{?el5}
echo "Encoding=UTF-8" >> %{buildroot}%{_datadir}/applications/x2gofm.desktop
%endif
desktop-file-validate %{buildroot}%{_datadir}/applications/x2gofm.desktop

%if 0%{?suse_version}
mkdir -p "%{buildroot}/%_sysconfdir/permissions.d"
cat > "%{buildroot}/%_sysconfdir/permissions.d/perl-X2Go-Server-DB" <<-EOF
%{_libdir}/x2go/libx2go-server-db-sqlite3-wrapper	root:x2gouser	02755
EOF
%endif

# Workaround for OpenSUSE 11 and SLE{S,D} 11:
# These versions do not support or have /etc/sudoers.d/.
# Installing a file in there will fail due to it being
# an orphaned directory and - worse - it will not work
# anyway.
# We move the file to the doc dir, add a README.sudo
# and also references this in our wiki on
# https://wiki.x2go.org/doku.php/doc:installation:x2goserver#workaround_for_qt-based_applications_and_sudo_kdesu
%if 0%{?suse_version} && 0%{?suse_version} < 1210
rm -f "%{buildroot}/etc/sudoers.d/x2goserver"
%endif

# Delete tmpfiles.d configuration file on systems
# not using systemd.
%if ( ! 0%{?fedora} ) && ( ( 0%{?rhel} && 0%{?rhel} < 7 ) || ( 0%{?suse_version} && 0%{?suse_version} < 1210 ) )
rm -f "%{buildroot}/%{_prefix}/lib/tmpfiles.d/x2goserver.conf"
%endif

# Dummy file - will be created/removed in post* scriptlets.
# We just need this here for the %ghost directory to work.
touch "%{buildroot}/%{_sysconfdir}/x2go/applications"


%pre common
if ! getent group x2gouser 1>/dev/null; then
    groupadd -r x2gouser
fi
if ! getent passwd x2gouser 1>/dev/null; then
    useradd -r -g x2gouser -d %{_localstatedir}/lib/x2go -s /sbin/nologin \
            -c "x2go" x2gouser
fi


%post
# Initialize the session database
if [ ! -s %{_localstatedir}/lib/x2go/x2go_sessions ]; then
  if grep -E "^backend=sqlite.*" /etc/x2go/x2gosql/sql 1>/dev/null 2>&1; then
      %{_sbindir}/x2godbadmin --createdb 1>/dev/null 2>&1 || :
  fi
fi

if grep -E "^backend=sqlite.*" /etc/x2go/x2gosql/sql 1>/dev/null 2>/dev/null; then
  if [ -s %{_localstatedir}/lib/x2go/x2go_sessions ]; then
    %{_sbindir}/x2godbadmin --updatedb 1>/dev/null 2>/dev/null || :
  fi
fi

# create /etc/x2go/applications symlink if not already there
# as a regular file, as a symlink, as a special file or as a directory
# N.B.: dangling symlinks will lead to test -e FAILING, because the symlink is dereferenced.
# This means that without the explicit symlink check (which doesn't dereference the last
# element), the ln call would be executed and fail, since the file/symlink already exists,
# leading to this scriptlet failing and hence leading to the installation failing.
if ! [ -e "%{_sysconfdir}/x2go/applications" ] && ! [ -L "%{_sysconfdir}/x2go/applications" ]; then
  ln -s "%{_datadir}/applications" "%{_sysconfdir}/x2go/applications"
fi

%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
%if 0%{?fedora} || 0%{?rhel} >= 7
%systemd_post x2goserver.service
%else
%service_add_post x2goserver.service
%endif
%else
# Do not use %{_sbindir} here. It's a macro for /usr/sbin.
/sbin/chkconfig --add x2goserver
/sbin/service x2goserver condrestart 1>/dev/null 2>&1 || :
%endif

%preun
if [ "${1}" = "0" ]; then
  if [ -L %{_sysconfdir}/x2go/applications ]; then
    rm -f %{_sysconfdir}/x2go/applications || :
  fi
fi

%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
%if 0%{?fedora} || 0%{?rhel} >= 7
%systemd_preun x2goserver.service

%postun
%systemd_postun_with_restart x2goserver.service
%else
%service_del_preun x2goserver.service

%pre
%service_add_pre x2goserver.service

%postun
%service_del_postun x2goserver.service
%endif
%else
if [ "$1" = 0 ]; then
  /sbin/service x2goserver stop 1>/dev/null 2>&1
  /sbin/chkconfig --del x2goserver
fi

%postun
if [ "$1" -ge "1" ] ; then
  /sbin/service x2goserver condrestart 1>/dev/null 2>&1 || :
fi
%endif


%post -n perl-X2Go-Server-DB
%if 0%{?suse_version}
%if 0%{?suse_version} <= 1130
%run_permissions
%else
%set_permissions %{_libdir}/x2go/libx2go-server-db-sqlite3-wrapper
%endif


%verifyscript -n perl-X2Go-Server-DB
%verify_permissions -e %{_libdir}/x2go/libx2go-server-db-sqlite3-wrapper
%endif


%post fmbindings
%if 0%{?suse_version} >= 1140
# Bug in SuSE's mime DB update script, work around it...
mkdir -p "/var/cache/gio-2.0"

%mime_database_post
%desktop_database_post
# We need the "weird" foo && foo < ... structure, because we only want to check the value
# *if* the macro is defined. Otherwise it will decay to 0 which means that the branch
# is always triggered - even on operating systems for which it should not be triggered.
# For example, this branch would be taken on Fedora >= 24 if using a plain "0%{?rhel} < 8"
# condition, since this ("0 < 8") would be true on a Fedora system.
%else
%if 0%{?suse_version} || ( 0%{?fedora} && 0%{?fedora} < 24 ) || ( 0%{?rhel} && 0%{?rhel} < 8 )
/usr/bin/update-mime-database %{_datadir}/mime &1>/dev/null 2>/dev/null || :
/usr/bin/update-desktop-database &1>/dev/null 2>/dev/null || :
%else
%if 0%{?fedora} && 0%{?fedora} < 25
/usr/bin/update-desktop-database &1>/dev/null 2>/dev/null || :
# FC 24 and higher have deprecated the mime database update scriptlet and handle changes transparently.
# FC 25 and higher have deprecated the desktop database update scriptlet and handle changes transparently.
%endif
%endif
%endif

%postun fmbindings
if [ $1 -eq 0 ] ; then
%if 0%{?suse_version} >= 1140
        mkdir -p "/var/cache/gio-2.0"

        %mime_database_postun
        %desktop_database_postun
%else
%if 0%{?suse_version} || ( 0%{?fedora} && 0%{?fedora} < 24 ) || ( 0%{?rhel} && 0%{?rhel} < 8 )
        /usr/bin/update-mime-database %{_datadir}/mime &1>/dev/null 2>/dev/null || :
        /usr/bin/update-desktop-database &1>/dev/null 2>/dev/null || :
%else
%if 0%{?fedora} && 0%{?fedora} < 25
        /usr/bin/update-desktop-database &1>/dev/null 2>/dev/null || :
        # Check the post scriptlet for more information.
%else
        # Need to have at least one command, do nothing.
        :
%endif
%endif
%endif
fi

%posttrans fmbindings
%if ( 0%{?fedora} && 0%{?fedora} < 24 ) || ( 0%{?rhel} && 0%{?rhel} < 8 )
/usr/bin/update-mime-database %{?fedora:-n} %{_datadir}/mime &> /dev/null || :
%endif


%pre printing
if ! getent group x2goprint 1>/dev/null; then
    groupadd -r x2goprint
fi
if ! getent passwd x2goprint 1>/dev/null; then
    useradd -r -g x2goprint -d /var/spool/x2goprint -s /sbin/nologin \
            -c "x2go" x2goprint
fi

%files
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
# Workaround for sudoers on OpenSUSE 11/SLE{S,D} 11.
%if 0%{?suse_version} && 0%{?suse_version} < 1210
%doc x2goserver/doc/README.sudoers
%doc x2goserver/etc/sudoers.d/x2goserver
%endif
%if ( ! 0%{?suse_version} ) || 0%{?suse_version} >= 1210
%config(noreplace) %{_sysconfdir}/sudoers.d/x2goserver
%endif
%{_bindir}/x2go*
%exclude %{_bindir}/x2goserver-run-extensions
%exclude %{_bindir}/x2gofm
%exclude %{_bindir}/x2goprint
%exclude %{_bindir}/x2goagent
%exclude %{_bindir}/x2go*-desktopsharing
%dir %{_libdir}/x2go
%{_libdir}/x2go/x2gochangestatus
%{_libdir}/x2go/x2gocheckport
%{_libdir}/x2go/x2gocreatesession
%{_libdir}/x2go/x2gocreateshadowsession
%{_libdir}/x2go/x2gogetagent
%{_libdir}/x2go/x2gogetagentstate
%{_libdir}/x2go/x2gogetdisplays
%{_libdir}/x2go/x2gogetfreeport
%{_libdir}/x2go/x2gogetports
%{_libdir}/x2go/x2gogetrandomport
%{_libdir}/x2go/x2gogetstatus
%{_libdir}/x2go/x2goinsertport
%{_libdir}/x2go/x2goinsertsession
%{_libdir}/x2go/x2goinsertshadowsession
%{_libdir}/x2go/x2goisint
%{_libdir}/x2go/x2goistrue
%{_libdir}/x2go/x2golistsessions_sql
%{_libdir}/x2go/x2gologlevel
%{_libdir}/x2go/x2goqueryconfig
%{_libdir}/x2go/x2goresume
%{_libdir}/x2go/x2gormforward
%{_libdir}/x2go/x2gormport
%{_libdir}/x2go/x2gosuspend-agent
%{_libdir}/x2go/x2gosyslog
%{_sbindir}/x2go*
%{_mandir}/man8/x2go*.8*
%{_mandir}/man1/x2go*.1*
%exclude %{_mandir}/man8/x2gofm.8*
%exclude %{_mandir}/man8/x2goprint.8*
%exclude %{_mandir}/man8/x2goserver-run-extensions.8*
%exclude %{_mandir}/man8/x2go*-desktopsharing.8*
%exclude %{_mandir}/man1/x2goagent.1*
%dir %{_datadir}/x2go/
%dir %{_datadir}/x2go/x2gofeature.d/
%{_datadir}/x2go/x2gofeature.d/x2goserver.features
%{_datadir}/x2go/versions/VERSION.x2goserver
%ghost %attr(0660,root,x2gouser) %{_localstatedir}/lib/x2go/x2go_sessions
%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
%{_unitdir}/x2goserver.service
%else
%if 0%{?el5}
%{_initrddir}/x2goserver
%else
%{_initddir}/x2goserver
%endif
%if 0%{?suse_version}
%{_sbindir}/rcx2goserver
%endif
%endif

%files -n perl-X2Go-Log
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%dir %{perl_vendorlib}/X2Go
%{perl_vendorlib}/X2Go/Log.pm
%{_mandir}/man3/X2Go::Log.*


%files -n perl-X2Go-Server
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%dir %{perl_vendorlib}/X2Go/Server
%{perl_vendorlib}/X2Go/Config.pm
%{perl_vendorlib}/X2Go/Server.pm
%{perl_vendorlib}/X2Go/SupeReNicer.pm
%{perl_vendorlib}/X2Go/Utils.pm
%{perl_vendorlib}/X2Go/Server/Agent*
%{_mandir}/man3/X2Go::Config.*
%{_mandir}/man3/X2Go::Server.*
%{_mandir}/man3/X2Go::SupeReNicer.*
%{_mandir}/man3/X2Go::Utils.*
%{_mandir}/man3/X2Go::Server::Agent.*
%{_mandir}/man3/X2Go::Server::Agent::*


%files -n perl-X2Go-Server-DB
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%dir %{_libdir}/x2go
%if 0%{?suse_version}
%config(noreplace) %{_sysconfdir}/permissions.d/perl-X2Go-Server-DB
%endif
%{perl_vendorlib}/X2Go/Server/DB*
%attr(2755,root,x2gouser) %{_libdir}/x2go/libx2go-server-db-sqlite3-wrapper
%{_libdir}/x2go/libx2go-server-db-sqlite3-wrapper.pl
%{_mandir}/man3/X2Go::Server::DB.*
%{_mandir}/man3/X2Go::Server::DB::*


%files common
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%attr(0775,root,x2gouser) %dir %{_localstatedir}/lib/x2go/
%dir %{_sysconfdir}/x2go/
%dir %{_sysconfdir}/x2go/x2gosql
%dir %{_sysconfdir}/x2go/x2gosql/passwords
%ghost %config(noreplace) %{_sysconfdir}/x2go/applications
%config(noreplace) %{_sysconfdir}/x2go/x2go_logout
%config(noreplace) %{_sysconfdir}/x2go/x2go_logout.d/
%config(noreplace) %{_sysconfdir}/x2go/x2goserver.conf
%config(noreplace) %{_sysconfdir}/x2go/x2gosql/sql
%if 0%{?fedora} || 0%{?rhel} >= 7 || 0%{?suse_version} >= 1210
%{_prefix}/lib/tmpfiles.d/x2goserver.conf
%endif
%{_mandir}/man5/x2goserver.conf.5.gz
%dir %{_datadir}/x2go/versions
%{_datadir}/x2go/versions/VERSION.x2goserver-common


%files desktopsharing
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_bindir}/x2go*-desktopsharing
%{_datadir}/x2go/versions/VERSION.x2goserver-desktopsharing
%{_datadir}/x2go/x2gofeature.d/x2goserver-desktopsharing.features
%{_mandir}/man8/x2go*-desktopsharing.8*
%dir %{_sysconfdir}/x2go/desktopsharing
%config(noreplace) %{_sysconfdir}/x2go/desktopsharing/settings


%files extensions
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_libdir}/x2go/extensions
%{_bindir}/x2goserver-run-extensions
%{_datadir}/x2go/x2gofeature.d/x2goserver-extensions.features
%{_datadir}/x2go/versions/VERSION.x2goserver-extensions
%{_mandir}/man8/x2goserver-run-extensions.8*


%files fmbindings
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_bindir}/x2gofm
%{_datadir}/applications/x2gofm.desktop
%{_datadir}/mime/packages/sshfs-x2go.xml
%{_datadir}/x2go/versions/VERSION.x2goserver-fmbindings
%{_datadir}/x2go/x2gofeature.d/x2goserver-fmbindings.features
%{_mandir}/man8/x2gofm.8*


%files x2goagent
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_bindir}/x2goagent
# %%{_libdir}/nx/bin/ is owned by nxagent package...
%{_libdir}/nx/bin/x2goagent
%{_datadir}/x2go/versions/VERSION.x2goserver-x2goagent
%{_datadir}/pixmaps/x2goagent.xpm
%{_datadir}/x2go/x2gofeature.d/x2goserver-x2goagent.features
%{_mandir}/man1/x2goagent.1*
%config(noreplace) %{_sysconfdir}/x2go/x2goagent.options
%config(noreplace) %{_sysconfdir}/x2go/keystrokes.cfg


%files x2goagent
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_datadir}/x2go/versions/VERSION.x2goserver-x2gokdrive
%{_datadir}/x2go/x2gofeature.d/x2goserver-x2gokdrive.features
%config(noreplace) %{_sysconfdir}/x2go/x2gokdrive.options


%files printing
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_bindir}/x2goprint
%{_datadir}/x2go/versions/VERSION.x2goserver-printing
%{_datadir}/x2go/x2gofeature.d/x2goserver-printing.features
%attr(0700,x2goprint,x2goprint) %{_localstatedir}/spool/x2goprint
%{_mandir}/man8/x2goprint.8*


%files xsession
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
%{_sysconfdir}/x2go/xinitrc.d
%if 0%{?fedora} || 0%{?rhel}
%{_sysconfdir}/x2go/Xclients.d
%endif
%{_sysconfdir}/x2go/Xresources
%config(noreplace) %{_sysconfdir}/x2go/Xsession
%{_datadir}/x2go/x2gofeature.d/x2goserver-xsession.features
%{_datadir}/x2go/versions/VERSION.x2goserver-xsession


%files logcheck
%defattr(-,root,root)
%doc debian/copyright
%doc debian/changelog
# logcheck is not available on OpenSUSE, SLES/SLED, FC19 and RHEL.
# Please re-check this periodically.
%if 0%{?suse_version} || ( 0%{?fedora} && 0%{?fedora} < 20 ) || 0%{?rhel}
%dir %{_sysconfdir}/logcheck
%dir %{_sysconfdir}/logcheck/ignore.d.server
%endif
%config(noreplace) %{_sysconfdir}/logcheck/ignore.d.server/x2goserver


%changelog
