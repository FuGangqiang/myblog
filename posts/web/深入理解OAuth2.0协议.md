created: 2015-08-17T18:04:00+08:00
tags: [OAuth, web, http]


注：本篇博客转载自：[http://blog.csdn.net/seccloud](http://blog.csdn.net/seccloud/article/details/8192707)


## 引言

如果你开车去酒店赴宴，你经常会苦于找不到停车位而耽误很多时间。
是否有好办法可以避免这个问题呢？有的，听说有一些豪车的车主就不担心这个问题。
豪车一般配备两种钥匙：主钥匙和泊车钥匙。
当你到酒店后，只需要将泊车钥匙交给服务生，停车的事情就由服务生去处理。
与主钥匙相比，这种泊车钥匙的使用功能是受限制的：
它只能启动发动机并让车行驶一段有限的距离，可以锁车，但无法打开后备箱，无法使用车内其他设备。
这里就体现了一种简单的“开放授权”思想：通过一把泊车钥匙，车主便能将汽车的部分使用功能（如启动发动机、行驶一段有限的距离）授权给服务生。

授权是一个古老的概念，它是一个多用户系统必须支持的功能特性。
比如，Alice 和 Bob 都是 Google 的用户，那么 Alice 应该可以将自己的照片授权给 Bob 访问。
但请注意到，这种授权是一种封闭授权，它只支持系统内部用户之间的相互授权，而不能支持与其他外部系统或用户之间的授权。
比如说，Alice 想使用“网易印像服务”将她的部分照片冲印出来，她怎么能做到呢？

肯定有人会说，Alice 可以将自己的 Google 用户名和密码告诉网易印像服务，事情不就解决了吗？
是的，但只有毫不关注安全和隐私的同学才会出此“绝招”。
那么我们就来想一想，这一“绝招”存在哪些问题？

1. 网易印像服务可能会缓存 Alice 的用户名和密码，而且可能没有加密保护。它一旦遭到攻击，Alice 就会躺着中枪。
2. 网易印像服务可以访问 Alice 在 Google 上的所有资源，Alice 无法对他们进行最小的权限控制，比如只允许访问某一张照片，1小时内访问有效。
3. Alice 无法撤消她的单个授权，除非 Alice 更新密码。

在以 Web 服务为核心的云计算时代，像用户 Alice 的这种授权需求变得日益迫切与兴盛，“开放授权(Open Authorization)”也正因此而生，
意在帮助 Alice 将她的资源授权给第三方应用，支持细粒度的权限控制，并且不会泄漏 Alice 的密码或其它认证凭据。

根据应用场景的不同，目前实现开放授权的方法分为两种：

1. 使用 OAuth 协议
2. 使用 IAM 服务

OAuth 协议主要适用于针对个人用户对资源的开放授权，比如 Google 的用户 Alice。
OAuth的特点是“现场授权”或“在线授权”：
客户端主要通过浏览器去访问资源，授权时需要认证 Alice 的资源所有者身份，并且需要 Alice 现场审批。
OAuth 一般在 SNS 服务中广泛使用，如微博。

IAM服务则不同，它的特点是“预先授权”或“离线授权”：
客户端主要通过 REST API 方式去访问资源，资源所有者可以预先知道第三方应用所需要的资源请求，一次授权之后，很少会变更。
IAM服务一般在云计算服务中使用，如 AWS 服务、阿里云计算服务。

本文主要介绍 OAuth 开放授权。关于以 IAM 服务提供的开放授权，我将在另一篇博文中介绍。下面我来介绍 OAuth 2.0 协议、协议的实例化描述、安全性分析。


## OAuth 2.0 协议

OAuth 2.0 是目前比较流行的做法，它率先被 Google, Yahoo, Microsoft, Facebook 等使用。
之所以标注为2.0，是因为最初有一个1.0协议，但这个1.0协议被弄得太复杂，易用性差，所以没有得到普及。
2.0是一个新的设计，协议简单清晰，但它并不兼容1.0，可以说与1.0没什么关系。
所以，我就只介绍2.0。


### 协议的参与者

从引言部分的描述我们可以看出，OAuth 的参与实体至少有如下三个：

* RO (resource owner): 资源所有者，对资源具有授权能力的人。如上文中的用户 Alice。
* RS (resource server): 资源服务器，它存储资源，并处理对资源的访问请求。如 Google 资源服务器，它所保管的资源就是用户 Alice 的照片。
* Client: 第三方应用，它获得 RO 的授权后便可以去访问 RO 的资源。如网易印像服务。

此外，为了支持开放授权功能以及更好地描述开放授权协议，OAuth 引入了第四个参与实体：

* AS (authorization server): 授权服务器，它认证 RO 的身份，为 RO 提供授权审批流程，并最终颁发授权令牌(Access Token)。
  读者请注意，为了便于协议的描述，这里只是在逻辑上把 AS 与 RS 区分开来；在物理上，AS 与 RS 的功能可以由同一个服务器来提供服务。


### 授权类型

在开放授权中，第三方应用(Client)可能是一个 Web 站点，也可能是在浏览器中运行的一段 JavaScript 代码，还可能是安装在本地的一个应用程序。
这些第三方应用都有各自的安全特性。
对于 Web 站点来说，它与 RO 浏览器是分离的，它可以自己保存协议中的敏感数据，这些密钥可以不暴露给 RO；
对于 JavaScript 代码和本地安全的应用程序来说，它本来就运行在 RO 的浏览器中，RO 是可以访问到 Client 在协议中的敏感数据。

OAuth 为了支持这些不同类型的第三方应用，提出了多种授权类型：

* 如授权码(Authorization Code Grant)
* 隐式授权(Implicit Grant)
* RO 凭证授权(Resource Owner Password Credentials Grant)
* Client 凭证授权 (Client Credentials Grant)

由于本文旨在帮助用户理解 OAuth 协议，
所以我将先介绍这些授权类型的基本思路，
然后选择其中最核心、最难理解、也是最广泛使用的一种授权类型——“授权码”，进行深入的介绍。

### OAuth 协议 - 基本思路

![Abstract_Protocol_Flow](/media/oauth/Abstract_Protocol_Flow.png)

如上图所示，协议的基本流程如下：

1. Client 请求 RO 的授权，请求中一般包含：要访问的资源路径，操作类型，Client 的身份等信息。
2. RO 批准授权，并将“授权证据”发送给 Client。至于 RO 如何批准，这个是协议之外的事情。
   典型的做法是，AS 提供授权审批界面，让 RO 显式批准。这个可以参考下一节实例化分析中的描述。
3. Client 向 AS 请求“访问令牌(Access Token)”。此时，Client 需向 AS 提供 RO 的“授权证据”，以及 Client 自己身份的凭证。
4. AS 验证通过后，向 Client 返回“访问令牌”。访问令牌也有多种类型，若为 bearer 类型，那么谁持有访问令牌，谁就能访问资源。
5. Client 携带“访问令牌”访问 RS 上的资源。在令牌的有效期内，Client 可以多次携带令牌去访问资源。
6. RS验证令牌的有效性，比如是否伪造、是否越权、是否过期，验证通过后，才能提供服务。


### 授权码类型的开放授权

![Abstract_Protocol_Flow](/media/oauth/Authorization_Code_Flow.png)

如上图所示，授权码类型的开放授权协议流程描述如下：

1. Client 初始化协议的执行流程。首先通过 HTTP 302 来重定向 RO 用户代理到 AS。
   Client 在 redirect_uri 中应包含如下参数：client_id, scope(描述被访问的资源), redirect_uri(即Client的URI), state(用于抵制CSRF攻击)。
   此外，请求中还可以包含 access_type 和 approval_prompt 参数。
   当 approval_prompt=force 时，AS 将提供交互页面，要求 RO 必须显式地批准（或拒绝）Client 的此次请求。
   如果没有 approval_prompt 参数，则默认为 RO 批准此次请求。
   当 access_type=offline 时，AS 将在颁发 access_token 时，同时还会颁发一个 refresh_token。
   因为 access_token 的有效期较短（如3600秒），为了优化协议执行流程，offline 方式将允许 Client 直接持 refresh_token 来换取一个新的 access_token。
2. AS 认证 RO 身份，并提供页面供 RO 决定是否批准或拒绝 Client 的此次请求（当 approval_prompt=force 时）。
3. 若请求被批准，AS 使用步骤1中 Client 提供的 redirect_uri 重定向 RO 用户代理到 Client。
   redirect_uri 须包含 authorization_code，以及步骤1中 Client 提供的 state。
   若请求被拒绝，AS 将通过 redirect_uri 返回相应的错误信息。
4. Client 拿 authorization_code 去访问 AS 以交换所需的 access_token。
   Client 请求信息中应包含用于认证 Client 身份所需的认证数据，以及上一步请求 authorization_code 时所用的 redirect_uri。
5. AS 在收到 authorization_code 时需要验证 Client 的身份，并验证收到的 redirect_uri 与第3步请求 authorization_code 时所使用的 redirect_uri 相匹配。
   如果验证通过，AS 将返回 access_token，以及 refresh_token（若 access_type=offline）。

如果读者对这个流程的细节不甚清楚，那么可以先看第3节的一个实例化描述，然后再回来看这部分内容。


##  OAuth协议实例化描述

下面我以实例化方式来帮助读者理解授权码类型的授权协议的运行过程。假设:

1. Alice 有一个有效的 Google 帐号；
2. Facebook.com 已经在 Google Authorization Server 上注册了 Client 身份，
   已经获得(client_id, client_secret)，注意 client_secret 是 Client 与 AS 之间的一个共享密钥。
3. Alice 想授权 Facebook.com 查看她的联系人列表(https://www.google.com/m8/feeds)。

下图展示了 Alice、Facebook.com、Google 资源服务器、以及 Google OAuth 授权服务器之间的协议运行过程：

![An Instance of Authorization Code Flow](/media/oauth/An_Instance_of_Authorization_Code_Flow.png)

协议所涉及到的细节都已经在上图上了，所以不打算再做详细介绍了。若看懂了此图，OAuth2.0 就理解了。

读者请注意，在步骤(4)中，Client 需要拿“授权码”去换“授权令牌”时，
Client 需要向AS证明自己的身份，即证明自己就是步骤(2)中 Alice 批准授权时的 Grantee。
这个身份证明的方法主要有两种（图3中使用了第1种）：

1. 通过 https 直接将 client_secret 发送给 AS，
   因为 client_secret 是由 Client 与 AS 所共享，所以只要传送 client_secret 的信道安全即可。
2. 通过消息认证码来认证 Client 身份，典型的算法有 HMAC-SHA1。
   在这种方式下，Client 无需传送 client_secret，只需发送消息请求的 signature 即可。
   由于不需要向 AS 传递敏感数据，所以它只需要使用 http 即可。

此外， 在步骤(2)中，Google 授权服务器需要认证 Alice 的 RO 身份，并提供授权界面给 Alice 进行授权审批。
今天 Google 提供的实例如下两图所示，仅供读者理解 OAuth 这种“现场授权”或“在线授权”的含义。

![RO's_Identity_Authentication](/media/oauth/ROs_Identity_Authentication.png)

![RO's_Authorization_Decision](/media/oauth/ROs_Authorization_Decision.png)


## OAuth设计上的安全性考虑


### 为何引入authorization_code？

协议设计中，为什么要使用 authorization_code 来交换 access_token？
这是读者容易想到的一个问题。也就是说，在协议的第3步，为什么不直接将 access_token 通过重定向方式返回给 Client 呢？
比如:

```
HTTP/1.1 302
Location:
https://www.facebook.com/?access_token=ya29.AHES6ZSXVKYTW2VAGZtnMjD&token_type=Bearer&expires_in=3600
```

如果直接返回 access_token，协议将变得更加简洁，而且少一次 Client 与 AS 之间的交互，性能也更优。
那为何不这么设计呢？协议文档中并没有给出这样设计的理由，但也不难分析：

1. 浏览器的 redirect_uri 是一个不安全信道，此方式不适合于传递敏感数据（如 access_token）。
   因为 uri 可能通过 HTTP referrer 被传递给其它恶意站点，也可能存在于浏览器 cacher 或 log 文件中，这就给攻击者盗取 access_token 带来了很多机会。
   另外，此协议也不应该假设 RO 用户代理的行为是可信赖的，因为 RO 的浏览器可能早已被攻击者植入了跨站脚本用来监听 access_token。
   因此，access_token 通过 RO 的用户代理传递给 Client，会显著扩大 access_token 被泄露的风险。
   但 authorization_code 可以通过 redirect_uri 方式来传递，是因为 authorization_code 并不像 access_token 一样敏感。
   即使 authorization_code 被泄露，攻击者也无法直接拿到 access_token，因为拿 authorization_code 去交换 access_token 是需要验证 Client 的真实身份。
   也就是说，除了 Client 之外，其他人拿 authorization_code 是没有用的。
   此外，access_token 应该只颁发给 Client 使用，其他任何主体（包括 RO）都不应该获取 access_token。
   协议的设计应能保证 Client 是唯一有能力获取 access_token 的主体。
   引入 authorization_code 之后，便可以保证 Client 是 access_token 的唯一持有人。
   当然，Client 也是唯一的有义务需要保护 access_token 不被泄露。
2. 引入 authorization_code 还会带来如下的好处。
   由于协议需要验证 Client 的身份，如果不引入 authorization_code，这个 Client 的身份认证只能通过第1步的 redirect_uri 来传递。
   同样由于 redirect_uri 是一个不安全信道，这就额外要求 Client 必须使用数字签名技术来进行身份认证，而不能用简单的密码或口令认证方式。
   引入 authorization_code 之后，AS 可以直接对 Client 进行身份认证（见步骤4和5），而且可以支持任意的 Client 认证方式（比如，简单地直接将 Client 端密钥发送给 AS）。

在我们理解了上述安全性考虑之后，读者也许会有豁然开朗的感觉，懂得了引入 authorization_code 的妙处。
那么，是不是一定要引入 authorization_code 才能解决这些安全问题呢？当然不是。
笔者将会在另一篇博文(转载者注：很可惜，这篇博文没有找到)给出一个直接返回 access_token 的扩展授权类型解决方案，它在满足相同安全性的条件下，使协议更简洁，交互次数更少。


### 基于Web安全的考虑

OAuth 协议设计不同于简单的网络安全协议的设计，
因为 OAuth 需要考虑各种 Web 攻击，比如 CSRF(Cross-Site Request Forgery), XSS(Cross Site Script), Clickjacking。
要理解这些攻击原理，读者需要对浏览器安全（eg, Same Origin Policy, 同源策略）有基本理解。
比如，在 redirect_uri 中引入 state 参数就是从浏览器安全角度考虑的，有了它就可以抵制 CSRF 攻击。
如果没有这个参数，攻击者便可以在 redirect_uri 中注入攻击者提供的 authorization_code 或 access_token，
结果可能导致 Client 访问错误的资源（比如，将款项汇到一个错误的帐号）。

基于 Web 安全的考虑，OAuth 协议文档中已经有了比较全面的阐述，所以我不打算在此文中进行展开。


## 结语

本文对 OAuth 2.0 开放授权协议及其设计上的安全性考虑做了一个基本的介绍，希望能给参与安全协议设计和开发的同学起到一点帮助。
