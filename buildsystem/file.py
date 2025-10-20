import shutil
import os
from log import log


def copy(src, dst):
    log(f"copy \n\tsrc: {src}, \n\tdst: {dst}", what="File")
    shutil.copyfile(src, dst)


def mkdir(path):
    log(f"make dir \n\tpath: {path}", what="File")
    os.mkdir(path)


def rmdir_ex(path):
    log(f"remove dir \n\tpath: {path}", what="File")
    if os.path.exists(path):
        shutil.rmtree(path)


def chmod(path, mode):
    log(f"chmod {mode} \n\tpath: {path}", what="File")
    os.chmod(path, mode)