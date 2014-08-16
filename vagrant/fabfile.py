# -*- coding: utf-8 -*-
"""Fabric file for installing requirements on OpenIndiana."""

from fabric.api import cd, env, path, run, shell_env, sudo

env.sudo_prefix = 'pfexec '

# TODO: install CouchDB
# TOOD?: install compress/xz so we can fetch the smallest source tarball
# TODO?: install Python 2.7
# TODO?: install pip using python2.7
# TODO?: install exifread using pip2.7

DIR_FOP = 'fop-1.1'
TAR_FOP = '{}-bin.tar.gz'.format(DIR_FOP)
URL_FOP = 'http://mirrors.sonic.net/apache/xmlgraphics/fop/binaries/{}'.format(TAR_FOP)
DIR_OTP = 'otp_src_17.1'
TAR_OTP = '{}.tar.gz'.format(DIR_OTP)
URL_OTP = 'http://www.erlang.org/download/{}'.format(TAR_OTP)


def install_erlang():
    """Install Erlang/OTP from source tarball."""
    install_tools()
    install_jdk()
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


def install_tools():
    """Install the developer tools packages."""
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


def install_jdk():
    """Install the Java Development Kit package."""
    _pkg_install('developer/java/jdk')


def _pkg_install(pkg):
    """Install the named package or list of packages.

    :type pkg: str|list
    :param pkg: name(s) of package(s) to install

    """
    if isinstance(pkg, list):
        pkg = ' '.join(pkg)
    sudo("pkg install -q {}".format(pkg))
