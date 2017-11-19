

# myproto #

Copyright (c) 2013-2017 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/myproto/master.svg)](https://travis-ci.org/altenwald/myproto)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/myproto.svg)](https://codecov.io/gh/altenwald/myproto)
[![License: EPL 1.1](https://img.shields.io/github/license/altenwald/myproto.svg)](https://raw.githubusercontent.com/altenwald/myproto/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/myproto.svg)](https://hex.pm/packages/myproto)

MySQL Server Protocol in Erlang. This project let you implement the MySQL protocol for your server. Throught a MySQL connection you could send queries or fake a MySQL connection to do a proxy or whatever else.


### <a name="Requirements">Requirements</a> ###

The system for tests use maps so, you have to use Erlang OTP 17+:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |
| 19.2 | :heavy_check_mark: | |
| 19.1 | :heavy_check_mark: | |
| 19.0 | :heavy_check_mark: | |
| 18.3 | :heavy_check_mark: | Recommended if you use OTP 18 |
| 18.2.1 | :heavy_check_mark: | |
| 18.2 | :heavy_check_mark: | |
| 18.1 | :heavy_check_mark: | |
| 18.0 | :heavy_check_mark: | |
| 17.5 | :heavy_check_mark: | Recommended if you use OTP 17 |
| 17.4 | :heavy_check_mark: | |
| 17.3 | :x: | fail in SSL |
| 17.1 | :heavy_check_mark: | |
| 17.0 | :heavy_check_mark: | |


### <a name="Usage">Usage</a> ###

If you want to use, only add this in rebar.config:

```erlang

  {deps, [
      {myproto, "0.3.1"}
  ]}.

```


### <a name="Configuration">Configuration</a> ###
For configuration you can check [configure and running](http://github.com/altenwald/myproto/blob/master/doc/config.md) entry.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/gen_myproto.md" class="module">gen_myproto</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/my_acceptor.md" class="module">my_acceptor</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/my_datatypes.md" class="module">my_datatypes</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/my_packet.md" class="module">my_packet</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/my_request.md" class="module">my_request</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/my_response.md" class="module">my_response</a></td></tr>
<tr><td><a href="http://github.com/altenwald/myproto/blob/master/doc/myproto_app.md" class="module">myproto_app</a></td></tr></table>

