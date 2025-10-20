import shutil
import os


def copy(src, dst):
    print(f"[File] copy \n\tsrc: {src}, \n\tdst: {dst}")
    shutil.copyfile(src, dst)


def mkdir(path):
    print(f"[File] make dir \n\tpath: {path}")
    os.mkdir(path)


def rmdir_ex(path):
    print(f"[File] remove dir \n\tpath: {path}")
    if os.path.exists(path):
        shutil.rmtree(path)