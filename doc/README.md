

# myproto #

Copyright (c) 2013-2020 Altenwald Solutions, S.L.

__Authors:__ "Manuel Rubio" ([`manuel@altenwald.com`](mailto:manuel@altenwald.com)).

[![Build Status](https://img.shields.io/travis/altenwald/myproto/master.svg)](https://travis-ci.org/altenwald/myproto)
[![Codecov](https://img.shields.io/codecov/c/github/altenwald/myproto.svg)](https://codecov.io/gh/altenwald/myproto)
[![License: EPL 1.1](https://img.shields.io/github/license/altenwald/myproto.svg)](https://raw.githubusercontent.com/altenwald/myproto/master/COPYING)
[![Hex](https://img.shields.io/hexpm/v/myproto.svg)](https://hex.pm/packages/myproto)

MySQL Server Protocol in Erlang. This project let you implement the MySQL protocol for your server. Throught a MySQL connection you could send queries or fake a MySQL connection to do a proxy or whatever else.


### <a name="Requirements">Requirements</a> ###

The system for tests use maps so, you have to use Erlang OTP 19.3+:

| Erlang Version | Support | Notes |
|:---|:---:|:---|
| 22.2 | :heavy_check_mark: | Recommended if you use OTP 22 |
| 22.1 | :heavy_check_mark: | |
| 22.0 | :heavy_check_mark: | |
| 21.3 | :heavy_check_mark: | Recommended if you use OTP 21 |
| 21.2 | :heavy_check_mark: | |
| 21.1 | :heavy_check_mark: | |
| 21.0 | :heavy_check_mark: | |
| 20.3 | :heavy_check_mark: | Recommended if you use OTP 20 |
| 20.2 | :heavy_check_mark: | |
| 20.1 | :heavy_check_mark: | |
| 20.0 | :heavy_check_mark: | |
| 19.3 | :heavy_check_mark: | Recommended if you use OTP 19 |


### <a name="Usage">Usage</a> ###

If you want to use, only add this in rebar.config:

```erlang

  {deps, [
      {myproto, "0.4.0"}
  ]}.

```


### <a name="Configuration">Configuration</a> ###
For configuration you can check [configure and running](doc/config.md) entry.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="gen_myproto.md" class="module">gen_myproto</a></td></tr>
<tr><td><a href="my_acceptor.md" class="module">my_acceptor</a></td></tr>
<tr><td><a href="my_datatypes.md" class="module">my_datatypes</a></td></tr>
<tr><td><a href="my_packet.md" class="module">my_packet</a></td></tr>
<tr><td><a href="my_request.md" class="module">my_request</a></td></tr>
<tr><td><a href="my_response.md" class="module">my_response</a></td></tr>
<tr><td><a href="myproto_app.md" class="module">myproto_app</a></td></tr></table>

