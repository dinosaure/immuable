# Immuable, a simple unikernel (in full OCaml) which serves files via HTTP

Immuable is a unikernel that allows content to be served via the http protocol
(http/1.1 and h2 via tls). The purpose of such a unikernel is to offer a simple
service (deploying a static website) at low cost:

The operating system weighs only ~7MB. The GCE `f1-micro` instance offers
0.6GB. The cost of such an instance (already oversized) is ~$4.69/month. We
recommend obtaining a server with KVM (for Linux) and deploying
[Albatross][albatross] to then deploy **multiple** unikernels.

The primary benefit of a unikernel is that it allows you to deploy an
application (such as Immuable, your static website) and run it in a **closed**
environment. A unikernel drastically reduces your application's attack surface,
which is its main advantage: it is more difficult to use a unikernel as an
auxiliary attack vector for your server.

However, the performance of a unikernel is **not** an advantage. We are talking
about a fairly minimal service with _reasonable_ performance, but if
"performance" is the issue, it is not advisable to use a unikernel (due to the
barrier that exists between the unikernel and the host system).

Immuable is therefore an example of a unikernel in OCaml. The advantage of using
OCaml rather than another language is the ability to integrate proven software
components and to have a language that is sufficiently "high-level" to be able
to develop a slightly more complex application.

In this case, all the protocols required by Immuable (from http to tcp,
including dns and tls) are implemented in OCaml. For more details, we invite you
to take a look at the work of [our cooperative][robur].

## How to use `immuable`

You can install immuable via OPAM, which will install two artefacts:
`immuable`, which is the software used to generate an _immuable_ archive, and
`immuable.hvt`, which is the unikernel for our Solo5 tender.
```shell
$ opam pin add https://github.com/dinosaure/immuable.git
$ command -v immuable > /dev/null
$ echo $?
0
```

Once Immuable is installed, the first thing to do is to create an archive of
your static website. For example, you might want to create a website from a
folder containing all your files (index.html, style.css, etc.):
```shell
$ test -d my_super_website
$ echo $?
0
$ immuable make -o pack.pack my_syper_website/
$ ls pack.pack
pack.pack
```

Next, you can launch the unikernel with this archive. There are currently two
ways to deploy a unikernel. You can do it directly using Solo5 (and
`solo5-hvt`) or using albatross.

### Deploy with Solo5

solo5-hvt

## How to build `immuable`

```shell
$ ./source.sh
$ dune build
$ cp _build/solo5/unikernel/main.exe immuable.hvt
$ sudo ip link add name service type bridge
$ sudo ip addr add 10.0.0.1/24 dev service
$ sudo ip tuntap add name tap0 mode tap
$ sudo ip link set tap0 master service
$ sudo ip link set service up
$ sudo ip link set tap0 up
$ dune exec bin/immuable.exe -- my-static-website/ -o block.pack
$ solo5-hvt --net:service=tap0 --block:immuable=block.pack -- immuable.hvt \
  --ipv4=10.0.0.2/24 --ipv4-gateway=10.0.0.1 \
  --color=always
```
