# Immuable, a simple unikernel (in full OCaml) which serves files via HTTP

Immuable is a unikernel that allows content to be served via the HTTP protocol
(only HTTP/1.1). The purpose of such a unikernel is to offer a simple service
(deploying a static website) at low cost:

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
"performance" is your primary issue, it is not advisable to use a unikernel (due
to the barrier that exists between the unikernel and the host system).

Immuable is therefore an example of a unikernel in OCaml. The advantage of using
OCaml rather than another language is the ability to integrate proven software
components and to have a language that is sufficiently "high-level" to be able
to develop a slightly more complex application.

In this case, all the protocols required by Immuable (from HTTP to TCP,
including DNS and TLS) are implemented in OCaml. For more details, we invite you
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

An unikernel is a full-fledged operating system that can be launched via KVM,
BHyve, or OpenVMM (on Linux, FreeBSD, or OpenBSD, respectively). In this case,
the unikernel you have just installed (`immuable.hvt`) is a [Solo5][solo5]
unikernel that can be run using the `solo5-hvt` tender. The "tender" is a
userspace executable that acts as a bridge between the unikernel and the host
system whenever it needs to communicate with the network (via an Ethernet
interface) or manage block devices. Immuable utilizes both of these devices.

The second device, the block device, has just been created: it is your archive,
a simple file resulting from the compression of all your static files. The
first device is an Ethernet interface, better known as a tap interface. You
must create one so that the unikernel can connect to a network. The simplest
way to do this is to:
- Create what is known as a bridge.
- Create a tap interface and "plug" it into the bridge.

```shell
$ sudo ip link add name service type bridge
$ sudo ip addr add 10.0.0.1/24 dev service
$ sudo ip tuntap add name tap0 mode tap
$ sudo ip link set tap0 master service
$ sudo ip link set service up
$ sudo ip link set tap0 up
```

You can use the `net.sh` script, which performs these operations on Linux.

Finally, once equipped with a tap interface and your archive file, you can
launch the immuable.hvt unikernel using Solo5 as follows:
```shell
$ solo5-hvt --net:service=tap0 --block:immuable=pack.pack -- \
  $(opam var bin)/immuable.hvt --ipv4=10.0.0.2/24 --ipv4-gateway=10.0.0.1
```

And your website will be available at http://10.0.0.2!

### Deploy with [Albatross][albatross]

Solo5 is fairly straightforward to use, but you might want to deploy your
unikernel to a remote server without manually managing the tap interface or the
archive. In this case, Albatross might be the right solution for you.

Albatross is a service that can be installed on systems like Debian to manage
and orchestrate unikernels. We recommend referring to the [official project
documentation][albatross-deploy] for instructions on how to install and deploy Albatross. Once set
up, you simply need to transfer your archive as a block device and then
transfer your unikernel:
```shell
$ albatross-client create-block \
  --ca user.pem --ca-key user.key --server-ca cacert.pem \
  --destination chameau.unikernel \
  --data=pack.pack archive $(du -m pack.pack|cut -f1)
$ albatross-client create \
  --ca user.pem --ca-key user.key --server-ca cacert.pem \
  --destination chameau.unikernel \
  immuable $(opam var bin)/immuable.hvt \
  --net service --block archive \
  --arg="--ipv4=10.0.0.2/24" --arg="--ipv4-gateway=10.0.0.1"
```

[solo5]: https://github.com/solo5/solo5
[robur]: https://robur.coop/
[albatross]: https://github.com/robur-coop/albatross
[albatross-deploy]: https://github.com/robur-coop/albatross?tab=readme-ov-file#setup
