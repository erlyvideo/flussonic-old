%define user flussonic
%define group flussonic

Summary: Flussonic multiprotocol streaming server
Name: flussonic
Version: 3.51
Release: 3%{?dist}
License: GPL
Group: Network
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root-%(%{__id_u} -n)
Source: http://debian.erlyvideo.org/tgz/flussonic-%{version}.tgz
URL: http://erlyvideo.org/
BuildRequires: erlang
Requires: erlang


%description 
Flussonic is a multiprotocol opensource videostreaming server
written with efficiency in mind


%prep
%setup -q


%build
make


%pre
%{_sbindir}/groupadd %{group}
%{_sbindir}/useradd -g %{group} -c "flussonic User" -d %{_localstatedir}/lib/%{name} -r -m %{user} 2>/dev/null || :


%install
rm -rf %{buildroot}

# Ownership will be changed after package install (see "post" section)
# So we comment it out in Makefile "install" section
sed -i 's|chown|#chown|' Makefile

# Also, init-script for RHEL/Centos MUST be installed to /etc/rc.d/init.d instead of /etc/init.d
# Otherwise there will be conflict with chkconfig package
sed -i 's|/etc/init.d/|/etc/rc.d/init.d/|' Makefile

make DESTROOT=%{buildroot} install


%post
/sbin/chkconfig --add %{name}
/sbin/chkconfig %{name} on


%files
%defattr(-, root, root)
%{_bindir}/*
%{_libdir}/erlang
%{_initrddir}/%{name}
%config(noreplace) %{_sysconfdir}/%{name}
%doc README.md
%attr(-, %{user}, %{group}) %{_localstatedir}/lib/%{name}
%attr(-, %{user}, %{group}) %{_localstatedir}/log/%{name}
%attr(-, %{user}, %{group}) %{_localstatedir}/cache/%{name}


%clean
rm -rf %{buildroot}


%changelog
* Mon Oct 8 2012 Max Lapshin <info@erlyvideo.org> - 3.51
- Initial build
