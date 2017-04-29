

# Module my_datatypes #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#binary_to_varchar-1">binary_to_varchar/1</a></td><td></td></tr><tr><td valign="top"><a href="#fix_integer_to_number-2">fix_integer_to_number/2</a></td><td></td></tr><tr><td valign="top"><a href="#number_to_fix_integer-2">number_to_fix_integer/2</a></td><td></td></tr><tr><td valign="top"><a href="#number_to_var_integer-1">number_to_var_integer/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_lenenc_string-1">read_lenenc_string/1</a></td><td></td></tr><tr><td valign="top"><a href="#string_nul_to_binary-1">string_nul_to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#var_integer_to_number-1">var_integer_to_number/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="binary_to_varchar-1"></a>

### binary_to_varchar/1 ###

<pre><code>
binary_to_varchar(Binary::binary() | null) -&gt; binary()
</code></pre>
<br />

<a name="fix_integer_to_number-2"></a>

### fix_integer_to_number/2 ###

<pre><code>
fix_integer_to_number(Size::integer(), Data::integer()) -&gt; integer()
</code></pre>
<br />

<a name="number_to_fix_integer-2"></a>

### number_to_fix_integer/2 ###

<pre><code>
number_to_fix_integer(Size::integer(), Data::binary()) -&gt; binary()
</code></pre>
<br />

<a name="number_to_var_integer-1"></a>

### number_to_var_integer/1 ###

<pre><code>
number_to_var_integer(Data::integer()) -&gt; binary()
</code></pre>
<br />

<a name="read_lenenc_string-1"></a>

### read_lenenc_string/1 ###

`read_lenenc_string(X1) -> any()`

<a name="string_nul_to_binary-1"></a>

### string_nul_to_binary/1 ###

<pre><code>
string_nul_to_binary(String::binary()) -&gt; binary()
</code></pre>
<br />

<a name="var_integer_to_number-1"></a>

### var_integer_to_number/1 ###

<pre><code>
var_integer_to_number(Var::binary()) -&gt; integer()
</code></pre>
<br />

