# -*- coding: utf-8 -*-
"""Fabric file for installing requirements on OpenIndiana.

First run `install_erlang` and then `install_couchdb`.

"""

from fabric.api import cd, env, path, put, run, shell_env, sudo

env.sudo_prefix = 'pfexec '

# TODO?: install compress/xz so we can fetch the smallest source tarball
# TODO?: install Python 2.7
# TODO?: install pip using python2.7
# TODO?: install exifread using pip2.7

DIR_FOP = 'fop-1.1'
TAR_FOP = '{}-bin.tar.gz'.format(DIR_FOP)
URL_FOP = 'http://mirrors.sonic.net/apache/xmlgraphics/fop/binaries/{}'.format(TAR_FOP)
DIR_OTP = 'otp_src_17.1'
TAR_OTP = '{}.tar.gz'.format(DIR_OTP)
URL_OTP = 'http://www.erlang.org/download/{}'.format(TAR_OTP)
DIR_CDB = 'apache-couchdb-1.6.0'
TAR_CDB = '{}.tar.gz'.format(DIR_CDB)
URL_CDB = 'http://mirrors.sonic.net/apache/couchdb/source/1.6.0/{}'.format(TAR_CDB)


def install_erlang():
    """Install Erlang/OTP from source tarball (installs tools & JDK)."""
    tools_pkgs = [
        'developer/illumos-gcc',
        'developer/gnu-binutils',
        'system/header',
        'system/library/math/header-math',
        'developer/library/lint',
        'compatibility/ucb'
    ]
    _pkg_install(tools_pkgs)
    # crle is idempotent, adding the same path twice is okay
    sudo('crle -u -l /opt/gcc/4.4.4/lib')
    _pkg_install('developer/java/jdk')
    # Fetch and extract Apache FOP binary tarball
    run('wget -q {}'.format(URL_FOP))
    run('tar zxf {}'.format(TAR_FOP))
    # Prepare to build Erlang/OTP from source
    run('wget -q {}'.format(URL_OTP))
    run('tar zxf {}'.format(TAR_OTP))
    paths = '/opt/gcc/4.4.4/bin:~/{}'.format(DIR_FOP)
    with cd(DIR_OTP), path(paths):
        run('./configure')
        run('make')
        sudo('make install')
        with shell_env(FOP_OPTS="-Xmx512m"):
            run('make docs')
            sudo('make install-docs')
    run('rm -rf {}* {}*'.format(DIR_FOP, DIR_OTP))


def install_couchdb():
    """Install Apache CouchDB and dependencies (requires Erlang/OTP)."""
    _pkg_install('developer/icu')
    # remove -lCrun from LDFLAGS in /usr/bin/icu-config
    icu_file = '/usr/bin/icu-config'
    lcrun_re = 's/^\(LDFLAGS=.*\) -lCrun\(.*\)$/\\1\\2/'
    sudo("sed -i.bak -e '{}' {}".format(lcrun_re, icu_file))
    sudo('pkg set-publisher -p http://pkg.openindiana.org/sfe')
    _pkg_install('runtime/javascript/spidermonkey')
    _pkg_install('library/nspr/header-nspr')
    run('wget -q {}'.format(URL_CDB))
    run('tar zxf {}'.format(TAR_CDB))
    path_addend = '/usr/local/bin:/opt/gcc/4.4.4/bin'
    with cd(DIR_CDB), path(path_addend):
        with shell_env(LDFLAGS='-L/usr/lib -L/usr/lib/mps'):
            run('./configure')
        run('make')
        sudo('make install')
        sudo('crle -u -l /usr/lib/mps')
    run('rm -rf {}*'.format(DIR_CDB))
    sudo('groupadd couchdb')
    sudo("useradd -c 'CouchDB User' -d /usr/local/var/lib/couchdb" +
         " -g couchdb -s /usr/bin/false couchdb")
    sudo('chown -R couchdb:couchdb /usr/local/var/lib/couchdb')
    sudo('chown -R couchdb:couchdb /usr/local/var/log/couchdb')
    sudo('chown -R couchdb:couchdb /usr/local/var/run/couchdb')
    ini_re = 's/^;bind_address = 127.0.0.1$/bind_address = 0.0.0.0/'
    ini_file = '/usr/local/etc/couchdb/local.ini'
    sudo("sed -i.bak -e '{}' {}".format(ini_re, ini_file))
    sudo('mkdir -p /var/svc/manifest/application/database')
    put('couchdb.xml', '/var/svc/manifest/application/database', use_sudo=True)
    sudo('chown -R root:sys /var/svc/manifest/application/database')
    sudo('svccfg -v import /var/svc/manifest/application/database/couchdb.xml')
    sudo('svcadm enable couch')


def _pkg_install(pkg):
    """Install the named package or list of packages.

    :type pkg: str|list
    :param pkg: name(s) of package(s) to install

    """
    if isinstance(pkg, list):
        pkg = ' '.join(pkg)
    sudo("pkg install -q {}".format(pkg))
