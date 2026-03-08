FROM ocaml/opam:ubuntu-25.04-ocaml-5.3

WORKDIR /workspace

RUN sudo apt update && \
    sudo apt install -y curl wget lsb-release software-properties-common gnupg2

RUN wget -qO- https://apt.llvm.org/llvm.sh | sudo bash -s -- 21
    
ENV LLVM_BIN="/usr/lib/llvm-18/bin/"
ENV PATH="${LLVM_BIN}:${PATH}"

ENV ENV OPAM_DISABLE_SANDBOXING=1
RUN opam update && opam install -vv -y dune utop ocaml-lsp-server ppx_deriving && \
    opam clean -a -c

RUN sudo apt update && \
    sudo apt install -y vim gdb rr
