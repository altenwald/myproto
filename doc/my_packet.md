

# Module my_packet #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_auth-1">decode_auth/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-1"></a>

### decode/1 ###

<pre><code>
decode(X1::binary()) -&gt; {ok, <a href="#type-response">response()</a>, binary()} | {more, binary()}
</code></pre>
<br />

<a name="decode_auth-1"></a>

### decode_auth/1 ###

<pre><code>
decode_auth(X1::binary()) -&gt; {ok, <a href="#type-user">user()</a>, binary()} | {more, binary()}
</code></pre>
<br />

<a name="encode-1"></a>

### encode/1 ###

`encode(Response) -> any()`

