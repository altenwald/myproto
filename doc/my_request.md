

# Module my_request #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_statem`](gen_statem.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#callback_mode-0">callback_mode/0</a></td><td></td></tr><tr><td valign="top"><a href="#check_clean_pass-2">check_clean_pass/2</a></td><td></td></tr><tr><td valign="top"><a href="#check_sha1_pass-2">check_sha1_pass/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_event-4">handle_event/4</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#sha1_hex-1">sha1_hex/1</a></td><td></td></tr><tr><td valign="top"><a href="#start-4">start/4</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-3">terminate/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_hex-1">to_hex/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="callback_mode-0"></a>

### callback_mode/0 ###

`callback_mode() -> any()`

<a name="check_clean_pass-2"></a>

### check_clean_pass/2 ###

<pre><code>
check_clean_pass(Pass::binary(), Salt::binary()) -&gt; binary()
</code></pre>
<br />

<a name="check_sha1_pass-2"></a>

### check_sha1_pass/2 ###

<pre><code>
check_sha1_pass(Pass::binary(), Salt::binary()) -&gt; binary()
</code></pre>
<br />

<a name="handle_event-4"></a>

### handle_event/4 ###

`handle_event(X1, Info, StateName, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="sha1_hex-1"></a>

### sha1_hex/1 ###

<pre><code>
sha1_hex(Data::binary()) -&gt; binary()
</code></pre>
<br />

<a name="start-4"></a>

### start/4 ###

<pre><code>
start(Socket::<a href="gen_tcp.md#type-socket">gen_tcp:socket()</a>, Id::pos_integer(), Handler::atom(), ParseQuery::boolean()) -&gt; {ok, pid()}
</code></pre>
<br />

<a name="terminate-3"></a>

### terminate/3 ###

`terminate(Reason, StateName, State) -> any()`

<a name="to_hex-1"></a>

### to_hex/1 ###

<pre><code>
to_hex(Hash::binary() | undefined) -&gt; binary()
</code></pre>
<br />

