[Federal Information Processing Standard](https://csrc.nist.gov/pubs/fips/140-2/upd2/final) (FIPS) 140-2 中描述了美国政府部门对于加密模块的安全要求，相当于国家制定的一个标准规范。

那 FIPS 跟我们有什么关系呢?如果跟美国的政府类的企业做软件或者提供相关服务，那么需要符合这样的规范，合规要求。如果是做基础软件服务，比如说做数据库的，它的用户可能涉及有政府类软件的供应商，于是相应的要求这些基础软件也支持 FIPS。

Go应用支持 FIPS 这块，还是有一些绕，记录一下。

首先，理论上只要有标准，应该会对应有很多的实现。而实际上，只有少量的官方认证库。openssl 支持，但是并不是所有版本，只有特定的版本是被官方认证过的。validated by an accredited [Cryptographic Module Validation Program (CVMP) laboratory](https://csrc.nist.gov/projects/testing-laboratories)。为什么会这样?可以认为是政治因素。要得到政府授权的官方认证，是需要相应的政治资源的，并不是说，我写了一个算法，实现了标准中要求的内容，然后我就是符合规范了，这并不是经过认证的。哪怕是 openssl 这种，也只是特定版本，而不是所有版本，可以认为"通过官方认证"的成本很高。可以类比为在中国，要某些政府机构给盖个章，成本很高。

红帽/google等这些特定的大公司会去搞这样的事情，而在个人开发者方面，都不会去做相应的开源实现，因为即使代码上实现了标准，也拿不到资源取得所谓官方认证。Go 的官方库没有实现对 FIPS 的支持，并且也不计划在未来会支持(我推测背后的原因，应该也是拿那个盖章比实现代码本身要复杂)。

OpenSSL 是通过认证过的。BoringSSL有部分加密模块是官方认证过的，但并不是全部。**所有这些认证过的库中，没有 Go 实现的。这就注定了 Go 程序如果要支持 FIPS，只有一种方式，那就是走 cgo 调用 C 模块认证过的那些库。**


然后，目前 Go 支持 FIPS 有两个选择，一个是使用红帽开源的[这个 Go 分支](https://github.com/golang-fips/go)，另一个是使用 google 的 [dev.boringcrypto 分支](https://go.googlesource.com/go/+/dev.boringcrypto/README.boringcrypto.md)，两者的共同点都是改了 Go 编译器。

红帽那个是它在整个红帽系统上支持，然后动态链接 openssl 的库的方式。给 Go 编译器打 patch 的问题是随着 Go 的版本的升级，这个 patch 是否稳定维护。另外一个 boringcrypto 则是 google 弄的，它也是从 Go 某个版本拉了一个分支，使用的不是 openssl 而是 google 自己维护的 boringssl。注意，boringssl 并不是整个都经过 FIPS 官方认证的，不过加密相关的部分模块是官方认证的。两个支持除了一个用的 openssl，一个用的 boringssl，另一点区别是红帽那个是动态链接，而 google 那个是静态链接的。

为什么两个实现都是要改 Go 编译器，而不能够提供一个三方库呢? 哪怕最终都必须使用 cgo 去调用 C 的实现，库也比改编译器要好呀。关于这个点我思考了一下，然后有一些推测：可能是模块依赖关系的特殊性。要支持 FIPS 需要部分的算法模块，如果我们用一个三方的小库来实现这些算法，我们没办法修改 Go 库那边 crypto/xxx 的代码来引用我们外部的加密算法。再然后更上层的，比如 tls 引用到 crypto/xxx 的库，再上层 http 引用 tls ... 这一层一层依赖关系上去，如果实现成三方库，没办法修改上层的引用让它依赖这个库。所以最终只有在编译器那层去改，才能保证最终的上层依赖都调用到修改过的 FIPS 加密模块。

关于如何选择哪种实现。最初 boringcrypto 是从 Go 拉的一个支持改的，是由于 google 内部也有 Go 的应用需要 FIPS 合规需求，然后 google 注意到外部可能也有这类需要，于是把分支开源出来了，但是声明由用户自己承担风险，不保证后续的随 Go 版本更新等等。红帽那边然后 fork 的 boringcrypto 的支持做了修改，有了 golang-fips 支持。cockroachdb 在做 FIPS 的事情的时候，使用的是 golang-fips 分支，那个时间点可能早一点。我发现在 1.19 之后，boringcrypto 已经进到 Go 的标准库源码里面去了(只是永远会是 expirement 的 feature)，所以这种应该更方便一些，只需要修改环境变量编译：

```
GOEXPERIMENT=boringcrypto go build .
```

最后是吐糟。这一类的合规真是跟开源世界相违备的。好的标准，是大家都涌跃去实现，然后出现了好多实现，各自有差异，大家正坐下来聊标准化的事情，产品是竞争力。而垄断则是由所谓的机构去定制标准，再售卖公信力，想跟我们政府机构做生意，嘿，得按我说了算，政府关系才是竞争力。

参考链接:

- https://kupczynski.info/posts/fips-golang/
- https://developers.redhat.com/articles/2022/05/31/your-go-application-fips-compliant
- https://projectcontour.io/guides/fips/
- https://github.com/openssl/openssl/blob/master/README-FIPS.md
- https://github.com/golang-fips/go
- https://banzaicloud.com/blog/istio-fips/
- https://groups.google.com/g/golang-dev/c/fqwZgtzHbzk
- https://go.googlesource.com/go/+/dev.boringcrypto/README.boringcrypto.md
- https://boringssl.googlesource.com/boringssl/+/master/crypto/fipsmodule/FIPS.md
