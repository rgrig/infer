# Copyright (c) 2015-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import logging
import os
import subprocess
import traceback

import util
from inferlib import config, utils


MODULE_NAME = __name__
MODULE_DESCRIPTION = '''Run analysis of code built with a command like:
xcodebuild [options]

Analysis examples:
infer -- xcodebuild -target HelloWorldApp -sdk iphonesimulator
infer -- xcodebuild -workspace HelloWorld.xcworkspace -scheme HelloWorld'''
LANG = ['clang']

CLANG_WRAPPER = os.path.join(config.WRAPPERS_DIRECTORY, 'clang')
CLANGPLUSPLUS_WRAPPER = os.path.join(config.WRAPPERS_DIRECTORY, 'clang++')


def gen_instance(*args):
    return XcodebuildCapture(*args)


create_argparser = util.base_argparser(MODULE_DESCRIPTION, MODULE_NAME)


class XcodebuildCapture:
    def __init__(self, args, cmd):
        self.args = args
        self.apple_clang_path = \
            subprocess.check_output(['xcrun', '--find', 'clang']).strip()

        xcode_version = util.run_cmd_ignore_fail(['xcodebuild', '-version'])
        apple_clang_version = util.run_cmd_ignore_fail([self.apple_clang_path,
                                                        '--version'])
        logging.info('Xcode version:\n%s', xcode_version)

        logging.info('clang version:\n%s', apple_clang_version)

        self.cmd = cmd

    def get_envvars(self):
        env_vars = utils.read_env()
        infer_args = env_vars['INFER_ARGS']
        if infer_args != '':
            infer_args += '^'  # '^' must be CommandLineOption.env_var_sep
        infer_args += '--fcp-apple-clang^' + self.apple_clang_path
        env_vars['INFER_ARGS'] = infer_args
        return env_vars

    def capture(self):
        # these settings will instruct xcodebuild on which clang to use
        self.cmd += ['CC={wrapper}'.format(wrapper=CLANG_WRAPPER)]
        self.cmd += [
            'CPLUSPLUS={wrapper}'.format(wrapper=CLANGPLUSPLUS_WRAPPER)]

        # skip the ProcessPCH phase to fix the "newer/older" incompatibility
        # error for the pch files generated by apple's clang and
        # the open-source one
        self.cmd += ['GCC_PRECOMPILE_PREFIX_HEADER=NO']

        try:
            env = utils.encode_env(self.get_envvars())
            cmd = map(utils.encode, self.cmd)
            subprocess.check_call(cmd, env=env)
            return os.EX_OK
        except subprocess.CalledProcessError as exc:
            if self.args.debug:
                traceback.print_exc()
            return exc.returncode
