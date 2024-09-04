## 如何访问本站

本站的域名使用了 [namespace 域名](https://namebase.io/)，而非传统的域名。因此通过传统的 DNS 域名解析方式无法直接访问到本站。

### 方式1: 通过 hns.to 跳转

[ns1.hns.to](https://ns1.hns.to)

该方式会直接跳转到网站 IP，有一定使用限制但是不需要额外操作。

限制比如：打开具体的页面如 [About](https://ns1.hns.to/about) 它不会跳转到到该页，而是跳转到主页。

### 方式2: 通过 DoH(DNS over HTTPS)

[blog.ns1/](http://blog.ns1/)

设置特定的 DoH 服务器可以支持解析 namebase 域名，比如 https://welcome.hnsdoh.com/

目前主流的操作系统和浏览器都支持了 DoH。以 chrome 为例：设置 -> 隐私和安全 -> 安全 -> 使用安全 DNS -> 添加自定义 DNS provider

填入 https://hnsdoh.com/dns-query

之后可以直接通过 blog.ns1 域名打开本站
