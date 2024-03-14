(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var $author$project$Main$LinkClicked = function (a) {
	return {$: 'LinkClicked', a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 'UrlChanged', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$Main$Bash = {$: 'Bash'};
var $author$project$Main$initialRunSpec = {assetDepends: _List_Nil, distribution: 'Ubuntu', execDepends: _List_Nil, file: 'src/script.sh', interpreter: $author$project$Main$Bash, release: '20.04', restartableEntryPoints: '', systemRequirements: $elm$core$Dict$empty, timeoutPolicy: $elm$core$Dict$empty, version: '0'};
var $author$project$Main$initialApp = {access: $elm$core$Maybe$Nothing, authorizedUsers: _List_Nil, categories: _List_Nil, description: '', developerNotes: '', developers: _List_Nil, dxapi: '0.0.1', httpsApp: $elm$core$Maybe$Nothing, inputSpec: _List_Nil, name: 'my_new_app', outputSpec: _List_Nil, regionalOptions: $elm$core$Dict$empty, runSpec: $author$project$Main$initialRunSpec, summary: 'App Summary', title: 'My New App', version: '0.0.1'};
var $rundis$elm_bootstrap$Bootstrap$Tab$Showing = {$: 'Showing'};
var $rundis$elm_bootstrap$Bootstrap$Tab$State = function (a) {
	return {$: 'State', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Tab$initialState = $rundis$elm_bootstrap$Bootstrap$Tab$State(
	{activeTab: $elm$core$Maybe$Nothing, visibility: $rundis$elm_bootstrap$Bootstrap$Tab$Showing});
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = F3(
	function (flags, url, key) {
		return _Utils_Tuple2(
			{accessSpecNetwork: '', app: $author$project$Main$initialApp, authorizedUser: '', customCategory: '', developer: '', editingExecDependsIndex: $elm$core$Maybe$Nothing, editingInputSpecIndex: $elm$core$Maybe$Nothing, editingOutputSpecIndex: $elm$core$Maybe$Nothing, editingSysReqHost: $elm$core$Maybe$Nothing, editingSysReqMinCores: 0, editingSysReqMinGpus: 0, editingSysReqMinMemory: 0, editingTimeoutPolicyIndex: $elm$core$Maybe$Nothing, enableAccess: false, enableHttpsApp: false, enableSysReqClusterDefinition: false, error: $elm$core$Maybe$Nothing, execDependsStage: '', execDependsToModify: $elm$core$Maybe$Nothing, httpsAppPort: $elm$core$Maybe$Nothing, incomingJson: $elm$core$Maybe$Nothing, inputSpecChoice: '', inputSpecPattern: '', inputSpecToModify: $elm$core$Maybe$Nothing, inputSpecToModifyDefault: '', inputSpecToModifyError: $elm$core$Maybe$Nothing, jsonError: $elm$core$Maybe$Nothing, key: key, outputSpecPattern: '', outputSpecToModify: $elm$core$Maybe$Nothing, outputSpecToModifyError: $elm$core$Maybe$Nothing, regionalOptionsResource: '', regionalOptionsToModify: $elm$core$Maybe$Nothing, sysReqToModify: $elm$core$Maybe$Nothing, tabState: $rundis$elm_bootstrap$Bootstrap$Tab$initialState, timeoutPolicyToModify: $elm$core$Maybe$Nothing, url: url},
			$elm$core$Platform$Cmd$none);
	});
var $author$project$Main$TabMsg = function (a) {
	return {$: 'TabMsg', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $rundis$elm_bootstrap$Bootstrap$Tab$subscriptions = F2(
	function (_v0, toMsg) {
		var state = _v0.a;
		var _v1 = state.visibility;
		if (_v1.$ === 'Start') {
			return $elm$browser$Browser$Events$onAnimationFrame(
				function (_v2) {
					return toMsg(
						$rundis$elm_bootstrap$Bootstrap$Tab$State(
							_Utils_update(
								state,
								{visibility: $rundis$elm_bootstrap$Bootstrap$Tab$Showing})));
				});
		} else {
			return $elm$core$Platform$Sub$none;
		}
	});
var $author$project$Main$subscriptions = function (model) {
	return A2($rundis$elm_bootstrap$Bootstrap$Tab$subscriptions, model.tabState, $author$project$Main$TabMsg);
};
var $author$project$Main$Apt = {$: 'Apt'};
var $author$project$Main$Aws = {$: 'Aws'};
var $author$project$Main$Azure = {$: 'Azure'};
var $author$project$Main$Cpan = {$: 'Cpan'};
var $author$project$Main$Cran = {$: 'Cran'};
var $author$project$Main$DefaultValBool = function (a) {
	return {$: 'DefaultValBool', a: a};
};
var $author$project$Main$DefaultValFloat = function (a) {
	return {$: 'DefaultValFloat', a: a};
};
var $author$project$Main$DefaultValInt = function (a) {
	return {$: 'DefaultValInt', a: a};
};
var $author$project$Main$DefaultValString = function (a) {
	return {$: 'DefaultValString', a: a};
};
var $author$project$Main$Gem = {$: 'Gem'};
var $author$project$Main$InputSpecApplet = {$: 'InputSpecApplet'};
var $author$project$Main$InputSpecArrayApplet = {$: 'InputSpecArrayApplet'};
var $author$project$Main$InputSpecArrayBoolean = {$: 'InputSpecArrayBoolean'};
var $author$project$Main$InputSpecArrayFile = {$: 'InputSpecArrayFile'};
var $author$project$Main$InputSpecArrayFloat = {$: 'InputSpecArrayFloat'};
var $author$project$Main$InputSpecArrayInt = {$: 'InputSpecArrayInt'};
var $author$project$Main$InputSpecArrayRecord = {$: 'InputSpecArrayRecord'};
var $author$project$Main$InputSpecArrayString = {$: 'InputSpecArrayString'};
var $author$project$Main$InputSpecBool = {$: 'InputSpecBool'};
var $author$project$Main$InputSpecFile = {$: 'InputSpecFile'};
var $author$project$Main$InputSpecFloat = {$: 'InputSpecFloat'};
var $author$project$Main$InputSpecHash = {$: 'InputSpecHash'};
var $author$project$Main$InputSpecInt = {$: 'InputSpecInt'};
var $author$project$Main$InputSpecRecord = {$: 'InputSpecRecord'};
var $author$project$Main$InputSpecString = {$: 'InputSpecString'};
var $author$project$Main$Pip = {$: 'Pip'};
var $author$project$Main$Python27 = {$: 'Python27'};
var $author$project$Main$Python3 = {$: 'Python3'};
var $author$project$Main$ResourceList = function (a) {
	return {$: 'ResourceList', a: a};
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$contains = _Regex_contains;
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $author$project$Main$App = function (name) {
	return function (title) {
		return function (summary) {
			return function (description) {
				return function (dxapi) {
					return function (version) {
						return function (developerNotes) {
							return function (categories) {
								return function (developers) {
									return function (authorizedUsers) {
										return function (runSpec) {
											return function (inputSpec) {
												return function (outputSpec) {
													return function (access) {
														return function (httpsApp) {
															return function (regionalOptions) {
																return {access: access, authorizedUsers: authorizedUsers, categories: categories, description: description, developerNotes: developerNotes, developers: developers, dxapi: dxapi, httpsApp: httpsApp, inputSpec: inputSpec, name: name, outputSpec: outputSpec, regionalOptions: regionalOptions, runSpec: runSpec, summary: summary, title: title, version: version};
															};
														};
													};
												};
											};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Main$AccessSpec = F5(
	function (network, project, allProjects, developer, projectCreation) {
		return {allProjects: allProjects, developer: developer, network: network, project: project, projectCreation: projectCreation};
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$json$Json$Decode$nullable = function (decoder) {
	return $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$null($elm$core$Maybe$Nothing),
				A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, decoder)
			]));
};
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder = F3(
	function (path, valDecoder, fallback) {
		var nullOr = function (decoder) {
			return $elm$json$Json$Decode$oneOf(
				_List_fromArray(
					[
						decoder,
						$elm$json$Json$Decode$null(fallback)
					]));
		};
		var handleResult = function (input) {
			var _v0 = A2(
				$elm$json$Json$Decode$decodeValue,
				A2($elm$json$Json$Decode$at, path, $elm$json$Json$Decode$value),
				input);
			if (_v0.$ === 'Ok') {
				var rawValue = _v0.a;
				var _v1 = A2(
					$elm$json$Json$Decode$decodeValue,
					nullOr(valDecoder),
					rawValue);
				if (_v1.$ === 'Ok') {
					var finalResult = _v1.a;
					return $elm$json$Json$Decode$succeed(finalResult);
				} else {
					return A2(
						$elm$json$Json$Decode$at,
						path,
						nullOr(valDecoder));
				}
			} else {
				return $elm$json$Json$Decode$succeed(fallback);
			}
		};
		return A2($elm$json$Json$Decode$andThen, handleResult, $elm$json$Json$Decode$value);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional = F4(
	function (key, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder,
				_List_fromArray(
					[key]),
				valDecoder,
				fallback),
			decoder);
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$decoderAccessSpec = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'projectCreation',
	$elm$json$Json$Decode$nullable($elm$json$Json$Decode$bool),
	$elm$core$Maybe$Nothing,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'developer',
		$elm$json$Json$Decode$nullable($elm$json$Json$Decode$bool),
		$elm$core$Maybe$Nothing,
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'allProjects',
			$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
			$elm$core$Maybe$Nothing,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'project',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
				$elm$core$Maybe$Nothing,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'network',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					$elm$json$Json$Decode$succeed($author$project$Main$AccessSpec))))));
var $author$project$Main$HttpsApp = F2(
	function (ports, sharedAccess) {
		return {ports: ports, sharedAccess: sharedAccess};
	});
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Main$decoderHttpsApp = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'shared_access',
	$elm$json$Json$Decode$string,
	'',
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'ports',
		$elm$json$Json$Decode$list($elm$json$Json$Decode$int),
		$elm$json$Json$Decode$succeed($author$project$Main$HttpsApp)));
var $author$project$Main$InputSpec = function (name) {
	return function (label) {
		return function (_class) {
			return function (optional) {
				return function (_default) {
					return function (patterns) {
						return function (choices) {
							return function (type_) {
								return function (help) {
									return function (group) {
										return function (suggestions) {
											return {choices: choices, _class: _class, _default: _default, group: group, help: help, label: label, name: name, optional: optional, patterns: patterns, suggestions: suggestions, type_: type_};
										};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $author$project$Main$decoderDefaultValue = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$andThen,
			function (f) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$DefaultValFloat(f));
			},
			$elm$json$Json$Decode$float),
			A2(
			$elm$json$Json$Decode$andThen,
			function (i) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$DefaultValInt(i));
			},
			$elm$json$Json$Decode$int),
			A2(
			$elm$json$Json$Decode$andThen,
			function (b) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$DefaultValBool(b));
			},
			$elm$json$Json$Decode$bool),
			A2(
			$elm$json$Json$Decode$andThen,
			function (s) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$DefaultValString(s));
			},
			$elm$json$Json$Decode$string)
		]));
var $elm$json$Json$Decode$fail = _Json_fail;
var $elm$core$String$toLower = _String_toLower;
var $author$project$Main$decoderInputSpecClass = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		var _v0 = $elm$core$String$toLower(str);
		switch (_v0) {
			case 'applet':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecApplet);
			case 'array:applet':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayApplet);
			case 'array:boolean':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayBoolean);
			case 'array:file':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayFile);
			case 'array:float':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayFloat);
			case 'array:int':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayInt);
			case 'array:record':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayRecord);
			case 'array:string':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecArrayString);
			case 'boolean':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecBool);
			case 'file':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecFile);
			case 'float':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecFloat);
			case 'hash':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecHash);
			case 'int':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecInt);
			case 'record':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecRecord);
			case 'string':
				return $elm$json$Json$Decode$succeed($author$project$Main$InputSpecString);
			default:
				return $elm$json$Json$Decode$fail('Unknown param type: ' + str);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Main$InputSpecSuggestion = F4(
	function (name, project, path, region) {
		return {name: name, path: path, project: project, region: region};
	});
var $author$project$Main$decoderInputSpecSuggestion = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'region',
	$elm$json$Json$Decode$string,
	'',
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'path',
		$elm$json$Json$Decode$string,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'project',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'name',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed($author$project$Main$InputSpecSuggestion)))));
var $author$project$Main$decoderInputSpec = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'suggestions',
	$elm$json$Json$Decode$list($author$project$Main$decoderInputSpecSuggestion),
	_List_Nil,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'group',
		$elm$json$Json$Decode$string,
		'',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'help',
			$elm$json$Json$Decode$string,
			'',
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'type',
				$elm$json$Json$Decode$nullable($elm$json$Json$Decode$string),
				$elm$core$Maybe$Nothing,
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'choices',
					$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
					_List_Nil,
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'patterns',
						$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
						_List_Nil,
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'default',
							$author$project$Main$decoderDefaultValue,
							$author$project$Main$DefaultValString(''),
							A4(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
								'optional',
								$elm$json$Json$Decode$bool,
								true,
								A4(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
									'class',
									$author$project$Main$decoderInputSpecClass,
									$author$project$Main$InputSpecString,
									A4(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
										'label',
										$elm$json$Json$Decode$string,
										'',
										A3(
											$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
											'name',
											$elm$json$Json$Decode$string,
											$elm$json$Json$Decode$succeed($author$project$Main$InputSpec))))))))))));
var $author$project$Main$OutputSpec = F6(
	function (name, label, help, optional, _class, patterns) {
		return {_class: _class, help: help, label: label, name: name, optional: optional, patterns: patterns};
	});
var $author$project$Main$decoderOutputSpec = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'patterns',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
	_List_Nil,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'class',
		$elm$json$Json$Decode$string,
		'',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'optional',
			$elm$json$Json$Decode$bool,
			true,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'help',
				$elm$json$Json$Decode$string,
				'',
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'label',
					$elm$json$Json$Decode$string,
					'',
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'name',
						$elm$json$Json$Decode$string,
						$elm$json$Json$Decode$succeed($author$project$Main$OutputSpec)))))));
var $author$project$Main$RegionalOptions = F2(
	function (resources, systemRequirements) {
		return {resources: resources, systemRequirements: systemRequirements};
	});
var $author$project$Main$ResourceString = function (a) {
	return {$: 'ResourceString', a: a};
};
var $author$project$Main$decoderRegionOptionsResource = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$andThen,
			function (str) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$ResourceString(str));
			},
			$elm$json$Json$Decode$string),
			A2(
			$elm$json$Json$Decode$andThen,
			function (vals) {
				return $elm$json$Json$Decode$succeed(
					$author$project$Main$ResourceList(vals));
			},
			$elm$json$Json$Decode$list($elm$json$Json$Decode$string))
		]));
var $author$project$Main$SystemRequirements = F2(
	function (instanceType, clusterSpec) {
		return {clusterSpec: clusterSpec, instanceType: instanceType};
	});
var $author$project$Main$ClusterSpec = F5(
	function (type_, version, initialInstanceCount, ports, bootstrapScript) {
		return {bootstrapScript: bootstrapScript, initialInstanceCount: initialInstanceCount, ports: ports, type_: type_, version: version};
	});
var $author$project$Main$decoderClusterSpec = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'bootstrapScript',
	$elm$json$Json$Decode$string,
	'',
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'ports',
		$elm$json$Json$Decode$string,
		'',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'initialInstanceCount',
			$elm$json$Json$Decode$int,
			1,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'version',
				$elm$json$Json$Decode$string,
				'',
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'type',
					$elm$json$Json$Decode$string,
					$elm$json$Json$Decode$succeed($author$project$Main$ClusterSpec))))));
var $author$project$Main$decoderSystemRequirementsBody = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'clusterSpec',
	A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $author$project$Main$decoderClusterSpec),
	$elm$core$Maybe$Nothing,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'instanceType',
		$elm$json$Json$Decode$string,
		$elm$json$Json$Decode$succeed($author$project$Main$SystemRequirements)));
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$json$Json$Decode$keyValuePairs = _Json_decodeKeyValuePairs;
var $elm$json$Json$Decode$dict = function (decoder) {
	return A2(
		$elm$json$Json$Decode$map,
		$elm$core$Dict$fromList,
		$elm$json$Json$Decode$keyValuePairs(decoder));
};
var $author$project$Main$decoderSystemRequirements = $elm$json$Json$Decode$dict($author$project$Main$decoderSystemRequirementsBody);
var $author$project$Main$decoderRegionalOptionsBody = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'systemRequirements',
	$author$project$Main$decoderSystemRequirements,
	$elm$core$Dict$empty,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'resources',
		$author$project$Main$decoderRegionOptionsResource,
		$author$project$Main$ResourceList(_List_Nil),
		$elm$json$Json$Decode$succeed($author$project$Main$RegionalOptions)));
var $author$project$Main$decoderRegionalOptions = $elm$json$Json$Decode$dict($author$project$Main$decoderRegionalOptionsBody);
var $author$project$Main$RunSpec = function (interpreter) {
	return function (file) {
		return function (distribution) {
			return function (release) {
				return function (version) {
					return function (restartableEntryPoints) {
						return function (execDepends) {
							return function (timeoutPolicy) {
								return function (systemRequirements) {
									return function (assetDepends) {
										return {assetDepends: assetDepends, distribution: distribution, execDepends: execDepends, file: file, interpreter: interpreter, release: release, restartableEntryPoints: restartableEntryPoints, systemRequirements: systemRequirements, timeoutPolicy: timeoutPolicy, version: version};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $author$project$Main$AssetDependsById = function (a) {
	return {$: 'AssetDependsById', a: a};
};
var $author$project$Main$AssetDependsByName = function (a) {
	return {$: 'AssetDependsByName', a: a};
};
var $author$project$Main$decoderAssetDependsById = A2($elm$json$Json$Decode$field, 'id', $elm$json$Json$Decode$string);
var $author$project$Main$decoderAssetDependsByName = A2($elm$json$Json$Decode$field, 'name', $elm$json$Json$Decode$string);
var $author$project$Main$decoderAssetDepends = $elm$json$Json$Decode$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$json$Json$Decode$map,
			function (val) {
				return $author$project$Main$AssetDependsById(
					{id: val});
			},
			$author$project$Main$decoderAssetDependsById),
			A2(
			$elm$json$Json$Decode$map,
			function (val) {
				return $author$project$Main$AssetDependsByName(
					{name: val});
			},
			$author$project$Main$decoderAssetDependsByName)
		]));
var $author$project$Main$ExecDepends = F4(
	function (name, packageManager, version, stages) {
		return {name: name, packageManager: packageManager, stages: stages, version: version};
	});
var $author$project$Main$decoderExecDependsPackageManager = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		var _v0 = $elm$core$String$toLower(str);
		switch (_v0) {
			case 'cpan':
				return $elm$json$Json$Decode$succeed($author$project$Main$Cpan);
			case 'cran':
				return $elm$json$Json$Decode$succeed($author$project$Main$Cran);
			case 'gem':
				return $elm$json$Json$Decode$succeed($author$project$Main$Gem);
			case 'pip':
				return $elm$json$Json$Decode$succeed($author$project$Main$Pip);
			default:
				return $elm$json$Json$Decode$succeed($author$project$Main$Apt);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Main$decoderExecDepends = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'stages',
	$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
	_List_Nil,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'version',
		$elm$json$Json$Decode$string,
		'',
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'package_manager',
			$author$project$Main$decoderExecDependsPackageManager,
			$author$project$Main$Apt,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'name',
				$elm$json$Json$Decode$string,
				$elm$json$Json$Decode$succeed($author$project$Main$ExecDepends)))));
var $author$project$Main$decoderRunSpecInterpreter = A2(
	$elm$json$Json$Decode$andThen,
	function (str) {
		var _v0 = $elm$core$String$toLower(str);
		switch (_v0) {
			case 'python2.7':
				return $elm$json$Json$Decode$succeed($author$project$Main$Python27);
			case 'python3':
				return $elm$json$Json$Decode$succeed($author$project$Main$Python3);
			default:
				return $elm$json$Json$Decode$succeed($author$project$Main$Bash);
		}
	},
	$elm$json$Json$Decode$string);
var $author$project$Main$TimeoutPolicyTime = F2(
	function (hours, minutes) {
		return {hours: hours, minutes: minutes};
	});
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalAt = F4(
	function (path, valDecoder, fallback, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A3($NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalDecoder, path, valDecoder, fallback),
			decoder);
	});
var $author$project$Main$decoderTimeoutPolicyBody = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalAt,
	_List_fromArray(
		['minutes']),
	$elm$json$Json$Decode$int,
	0,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optionalAt,
		_List_fromArray(
			['hours']),
		$elm$json$Json$Decode$int,
		0,
		$elm$json$Json$Decode$succeed($author$project$Main$TimeoutPolicyTime)));
var $author$project$Main$decoderTimeoutPolicy = $elm$json$Json$Decode$dict($author$project$Main$decoderTimeoutPolicyBody);
var $author$project$Main$decoderRunSpec = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'assetDepends',
	$elm$json$Json$Decode$list($author$project$Main$decoderAssetDepends),
	_List_Nil,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'systemRequirements',
		$author$project$Main$decoderSystemRequirements,
		$elm$core$Dict$empty,
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'timeoutPolicy',
			$author$project$Main$decoderTimeoutPolicy,
			$elm$core$Dict$empty,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'execDepends',
				$elm$json$Json$Decode$list($author$project$Main$decoderExecDepends),
				_List_Nil,
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'restartableEntryPoints',
					$elm$json$Json$Decode$string,
					'',
					A4(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
						'version',
						$elm$json$Json$Decode$string,
						'',
						A3(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
							'release',
							$elm$json$Json$Decode$string,
							A3(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
								'distribution',
								$elm$json$Json$Decode$string,
								A3(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
									'file',
									$elm$json$Json$Decode$string,
									A3(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
										'interpreter',
										$author$project$Main$decoderRunSpecInterpreter,
										$elm$json$Json$Decode$succeed($author$project$Main$RunSpec)))))))))));
var $author$project$Main$decoderApp = A4(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
	'regionalOptions',
	$author$project$Main$decoderRegionalOptions,
	$elm$core$Dict$empty,
	A4(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
		'httpsApp',
		A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $author$project$Main$decoderHttpsApp),
		$elm$core$Maybe$Nothing,
		A4(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
			'access',
			A2($elm$json$Json$Decode$map, $elm$core$Maybe$Just, $author$project$Main$decoderAccessSpec),
			$elm$core$Maybe$Nothing,
			A4(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
				'outputSpec',
				$elm$json$Json$Decode$list($author$project$Main$decoderOutputSpec),
				_List_Nil,
				A4(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
					'inputSpec',
					$elm$json$Json$Decode$list($author$project$Main$decoderInputSpec),
					_List_Nil,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'runSpec',
						$author$project$Main$decoderRunSpec,
						A4(
							$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
							'authorizedUsers',
							$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
							_List_Nil,
							A4(
								$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
								'developers',
								$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
								_List_Nil,
								A4(
									$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
									'categories',
									$elm$json$Json$Decode$list($elm$json$Json$Decode$string),
									_List_Nil,
									A4(
										$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
										'developerNotes',
										$elm$json$Json$Decode$string,
										'',
										A4(
											$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
											'version',
											$elm$json$Json$Decode$string,
											'',
											A4(
												$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
												'dxapi',
												$elm$json$Json$Decode$string,
												'',
												A4(
													$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$optional,
													'description',
													$elm$json$Json$Decode$string,
													'',
													A3(
														$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
														'summary',
														$elm$json$Json$Decode$string,
														A3(
															$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
															'title',
															$elm$json$Json$Decode$string,
															A3(
																$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
																'name',
																$elm$json$Json$Decode$string,
																$elm$json$Json$Decode$succeed($author$project$Main$App)))))))))))))))));
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$Main$defaultValueToString = function (val) {
	switch (val.$) {
		case 'DefaultValInt':
			var i = val.a;
			return $elm$core$String$fromInt(i);
		case 'DefaultValFloat':
			var f = val.a;
			return $elm$core$String$fromFloat(f);
		case 'DefaultValBool':
			var b = val.a;
			return b ? 'True' : 'False';
		default:
			var v = val.a;
			return v;
	}
};
var $elm$core$Debug$log = _Debug_log;
var $elm$core$Basics$round = _Basics_round;
var $elm$core$String$toFloat = _String_toFloat;
var $elm$core$String$toUpper = _String_toUpper;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Main$decodeIncomingJson = function (model) {
	var setDefault = function (spec) {
		var newDefault = function () {
			var _v6 = _Utils_Tuple2(spec._default, spec._class);
			_v6$9:
			while (true) {
				switch (_v6.a.$) {
					case 'DefaultValFloat':
						switch (_v6.b.$) {
							case 'InputSpecFloat':
								var _v8 = _v6.b;
								return spec._default;
							case 'InputSpecInt':
								var f = _v6.a.a;
								var _v9 = _v6.b;
								return $author$project$Main$DefaultValInt(
									$elm$core$Basics$round(f));
							default:
								break _v6$9;
						}
					case 'DefaultValInt':
						switch (_v6.b.$) {
							case 'InputSpecFloat':
								var i = _v6.a.a;
								var _v10 = _v6.b;
								return $author$project$Main$DefaultValFloat(i);
							case 'InputSpecInt':
								var _v11 = _v6.b;
								return spec._default;
							default:
								break _v6$9;
						}
					case 'DefaultValBool':
						if (_v6.b.$ === 'InputSpecBool') {
							var _v12 = _v6.b;
							return spec._default;
						} else {
							break _v6$9;
						}
					default:
						switch (_v6.b.$) {
							case 'InputSpecString':
								var _v7 = _v6.b;
								return spec._default;
							case 'InputSpecInt':
								var s = _v6.a.a;
								var _v13 = _v6.b;
								return $author$project$Main$DefaultValInt(
									A2(
										$elm$core$Maybe$withDefault,
										0,
										$elm$core$String$toInt(s)));
							case 'InputSpecFloat':
								var s = _v6.a.a;
								var _v14 = _v6.b;
								return $author$project$Main$DefaultValFloat(
									A2(
										$elm$core$Maybe$withDefault,
										0.0,
										$elm$core$String$toFloat(s)));
							case 'InputSpecBool':
								var s = _v6.a.a;
								var _v15 = _v6.b;
								return $author$project$Main$DefaultValBool(
									$elm$core$String$toUpper(s) === 'TRUE');
							default:
								break _v6$9;
						}
				}
			}
			return $author$project$Main$DefaultValString(
				$author$project$Main$defaultValueToString(spec._default));
		}();
		var _v3 = A2($elm$core$Debug$log, 'default name', spec.name);
		var _v4 = A2($elm$core$Debug$log, 'default value', spec._default);
		var _v5 = A2($elm$core$Debug$log, 'default class', spec._class);
		return _Utils_update(
			spec,
			{_default: newDefault});
	};
	var _v0 = function () {
		var _v1 = model.incomingJson;
		if (_v1.$ === 'Just') {
			var json = _v1.a;
			var _v2 = A2($elm$json$Json$Decode$decodeString, $author$project$Main$decoderApp, json);
			if (_v2.$ === 'Ok') {
				var app = _v2.a;
				var inputSpecWithDefaults = A2($elm$core$List$map, setDefault, app.inputSpec);
				return _Utils_Tuple2(
					_Utils_update(
						app,
						{inputSpec: inputSpecWithDefaults}),
					$elm$core$Maybe$Nothing);
			} else {
				var e = _v2.a;
				return _Utils_Tuple2(
					model.app,
					$elm$core$Maybe$Just(
						$elm$json$Json$Decode$errorToString(e)));
			}
		} else {
			return _Utils_Tuple2(
				model.app,
				$elm$core$Maybe$Just('No JSON'));
		}
	}();
	var newApp = _v0.a;
	var err = _v0.b;
	return _Utils_update(
		model,
		{app: newApp, jsonError: err});
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$json$Json$Encode$dict = F3(
	function (toKey, toValue, dictionary) {
		return _Json_wrap(
			A3(
				$elm$core$Dict$foldl,
				F3(
					function (key, value, obj) {
						return A3(
							_Json_addField,
							toKey(key),
							toValue(value),
							obj);
					}),
				_Json_emptyObject(_Utils_Tuple0),
				dictionary));
	});
var $elm$core$Dict$isEmpty = function (dict) {
	if (dict.$ === 'RBEmpty_elm_builtin') {
		return true;
	} else {
		return false;
	}
};
var $author$project$Main$dictToMaybe = function (d) {
	return $elm$core$Dict$isEmpty(d) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(d);
};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $the_sett$json_optional$Json$Encode$Optional$WithValue = F2(
	function (a, b) {
		return {$: 'WithValue', a: a, b: b};
	});
var $the_sett$json_optional$Json$Encode$Optional$field = F2(
	function (encoder, _v0) {
		var name = _v0.a;
		var val = _v0.b;
		return A2(
			$the_sett$json_optional$Json$Encode$Optional$WithValue,
			name,
			encoder(val));
	});
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $the_sett$json_optional$Json$Encode$Optional$objectMaySkip = function (fields) {
	return $elm$json$Json$Encode$object(
		A3(
			$elm$core$List$foldr,
			F2(
				function (fld, accum) {
					switch (fld.$) {
						case 'WithValue':
							var name = fld.a;
							var val = fld.b;
							return A2(
								$elm$core$List$cons,
								_Utils_Tuple2(name, val),
								accum);
						case 'Optional':
							var name = fld.a;
							return accum;
						case 'Nullable':
							var name = fld.a;
							return A2(
								$elm$core$List$cons,
								_Utils_Tuple2(name, $elm$json$Json$Encode$null),
								accum);
						default:
							return accum;
					}
				}),
			_List_Nil,
			fields));
};
var $the_sett$json_optional$Json$Encode$Optional$Optional = function (a) {
	return {$: 'Optional', a: a};
};
var $the_sett$json_optional$Json$Encode$Optional$optionalField = F2(
	function (encoder, _v0) {
		var name = _v0.a;
		var maybeVal = _v0.b;
		if (maybeVal.$ === 'Just') {
			var val = maybeVal.a;
			return A2(
				$the_sett$json_optional$Json$Encode$Optional$WithValue,
				name,
				encoder(val));
		} else {
			return $the_sett$json_optional$Json$Encode$Optional$Optional(name);
		}
	});
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Main$encodeAccessSpec = function (access) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2('network', access.network)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('project', access.project)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('allProjects', access.allProjects)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$bool,
				_Utils_Tuple2('developer', access.developer)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$bool,
				_Utils_Tuple2('projectCreation', access.projectCreation))
			]));
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $author$project$Main$encodeHttpsApp = function (httpsApp) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'ports',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$int, httpsApp.ports)),
				_Utils_Tuple2(
				'shared_access',
				$elm$json$Json$Encode$string(httpsApp.sharedAccess))
			]));
};
var $author$project$Main$defaultValueToMaybe = function (_default) {
	if (_default.$ === 'DefaultValString') {
		var val = _default.a;
		return $elm$core$String$isEmpty(val) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(_default);
	} else {
		return $elm$core$Maybe$Just(_default);
	}
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $author$project$Main$encodeDefault = function (_default) {
	switch (_default.$) {
		case 'DefaultValInt':
			var val = _default.a;
			return $elm$json$Json$Encode$int(val);
		case 'DefaultValFloat':
			var val = _default.a;
			return $elm$json$Json$Encode$float(val);
		case 'DefaultValBool':
			var val = _default.a;
			return $elm$json$Json$Encode$bool(val);
		default:
			var val = _default.a;
			return $elm$json$Json$Encode$string(val);
	}
};
var $author$project$Main$stringToMaybe = function (s) {
	return $elm$core$String$isEmpty(s) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(s);
};
var $author$project$Main$encodeInputSpecSuggestion = function (suggestion) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('name', suggestion.name)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('project', suggestion.project)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('path', suggestion.path)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'region',
					$author$project$Main$stringToMaybe(suggestion.region)))
			]));
};
var $author$project$Main$inputSpecClassToString = function (_class) {
	switch (_class.$) {
		case 'InputSpecApplet':
			return 'applet';
		case 'InputSpecArrayApplet':
			return 'array:applet';
		case 'InputSpecArrayBoolean':
			return 'array:boolean';
		case 'InputSpecArrayFile':
			return 'array:file';
		case 'InputSpecArrayFloat':
			return 'array:float';
		case 'InputSpecArrayInt':
			return 'array:int';
		case 'InputSpecArrayRecord':
			return 'array:record';
		case 'InputSpecArrayString':
			return 'array:string';
		case 'InputSpecBool':
			return 'boolean';
		case 'InputSpecFile':
			return 'file';
		case 'InputSpecFloat':
			return 'float';
		case 'InputSpecHash':
			return 'hash';
		case 'InputSpecInt':
			return 'int';
		case 'InputSpecRecord':
			return 'record';
		default:
			return 'string';
	}
};
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $author$project$Main$listToMaybe = function (vals) {
	return $elm$core$List$isEmpty(vals) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(vals);
};
var $author$project$Main$encodeInputSpec = function (inputSpec) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('name', inputSpec.name)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('label', inputSpec.label)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'class',
					$author$project$Main$inputSpecClassToString(inputSpec._class))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$bool,
				_Utils_Tuple2('optional', inputSpec.optional)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$author$project$Main$encodeDefault,
				_Utils_Tuple2(
					'default',
					$author$project$Main$defaultValueToMaybe(inputSpec._default))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2(
					'patterns',
					$author$project$Main$listToMaybe(inputSpec.patterns))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2(
					'choices',
					$author$project$Main$listToMaybe(inputSpec.choices))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('type', inputSpec.type_)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('help', inputSpec.help)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('group', inputSpec.group)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($author$project$Main$encodeInputSpecSuggestion),
				_Utils_Tuple2(
					'suggestions',
					$author$project$Main$listToMaybe(inputSpec.suggestions)))
			]));
};
var $author$project$Main$encodeOutputSpec = function (spec) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('name', spec.name)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'label',
					$author$project$Main$stringToMaybe(spec.label))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'help',
					$author$project$Main$stringToMaybe(spec.help))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$bool,
				_Utils_Tuple2('optional', spec.optional)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('class', spec._class)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2(
					'patterns',
					$author$project$Main$listToMaybe(spec.patterns)))
			]));
};
var $author$project$Main$encodeClusterSpec = function (spec) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(spec.type_)),
				_Utils_Tuple2(
				'version',
				$elm$json$Json$Encode$string(spec.version)),
				_Utils_Tuple2(
				'initialInstanceCount',
				$elm$json$Json$Encode$int(spec.initialInstanceCount)),
				_Utils_Tuple2(
				'ports',
				$elm$json$Json$Encode$string(spec.ports)),
				_Utils_Tuple2(
				'bootstrapScript',
				$elm$json$Json$Encode$string(spec.bootstrapScript))
			]));
};
var $author$project$Main$encodeSystemRequirements = function (req) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('instanceType', req.instanceType)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$author$project$Main$encodeClusterSpec,
				_Utils_Tuple2('clusterSpec', req.clusterSpec))
			]));
};
var $author$project$Main$encodeRegionalOptions = function (opts) {
	var resources = function () {
		var _v0 = opts.resources;
		if (_v0.$ === 'ResourceString') {
			var s = _v0.a;
			return _List_fromArray(
				[s]);
		} else {
			var vals = _v0.a;
			return vals;
		}
	}();
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				A2($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Main$encodeSystemRequirements),
				_Utils_Tuple2('systemRequirements', opts.systemRequirements)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2(
					'resources',
					$author$project$Main$listToMaybe(resources)))
			]));
};
var $author$project$Main$encodeAssetDepends = function (assetDepends) {
	if (assetDepends.$ === 'AssetDependsById') {
		var asset = assetDepends.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'id',
					$elm$json$Json$Encode$string(asset.id))
				]));
	} else {
		var asset = assetDepends.a;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(asset.name))
				]));
	}
};
var $author$project$Main$packageManagerToString = function (packageManager) {
	switch (packageManager.$) {
		case 'Apt':
			return 'apt';
		case 'Cpan':
			return 'cpan';
		case 'Cran':
			return 'cran';
		case 'Gem':
			return 'gem';
		default:
			return 'pip';
	}
};
var $author$project$Main$encodeExecDepends = function (execDepends) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('name', execDepends.name)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'package_manager',
					$author$project$Main$packageManagerToString(execDepends.packageManager))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'version',
					$author$project$Main$stringToMaybe(execDepends.version))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
				_Utils_Tuple2(
					'stages',
					$author$project$Main$listToMaybe(execDepends.stages)))
			]));
};
var $author$project$Main$encodeTimeoutPolicyTime = function (policy) {
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'hours',
				$elm$json$Json$Encode$int(policy.hours)),
				_Utils_Tuple2(
				'minutes',
				$elm$json$Json$Encode$int(policy.minutes))
			]));
};
var $author$project$Main$runSpecInterpreterToString = function (interpreter) {
	switch (interpreter.$) {
		case 'Bash':
			return 'bash';
		case 'Python27':
			return 'python2.7';
		default:
			return 'python3';
	}
};
var $author$project$Main$encodeRunSpec = function (runSpec) {
	return $the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
		_List_fromArray(
			[
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'interpreter',
					$author$project$Main$runSpecInterpreterToString(runSpec.interpreter))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('file', runSpec.file)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('distribution', runSpec.distribution)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('release', runSpec.release)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$field,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2('version', runSpec.version)),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$string,
				_Utils_Tuple2(
					'restartableEntryPoints',
					$author$project$Main$stringToMaybe(runSpec.restartableEntryPoints))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($author$project$Main$encodeExecDepends),
				_Utils_Tuple2(
					'execDepends',
					$author$project$Main$listToMaybe(runSpec.execDepends))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				A2($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Main$encodeTimeoutPolicyTime),
				_Utils_Tuple2(
					'timeoutPolicy',
					$author$project$Main$dictToMaybe(runSpec.timeoutPolicy))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				A2($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Main$encodeSystemRequirements),
				_Utils_Tuple2(
					'systemRequirements',
					$author$project$Main$dictToMaybe(runSpec.systemRequirements))),
				A2(
				$the_sett$json_optional$Json$Encode$Optional$optionalField,
				$elm$json$Json$Encode$list($author$project$Main$encodeAssetDepends),
				_Utils_Tuple2(
					'assetDepends',
					$author$project$Main$listToMaybe(runSpec.assetDepends)))
			]));
};
var $author$project$Main$encodeApp = function (app) {
	var appDescription = $elm$core$String$isEmpty(app.description) ? app.name : app.description;
	return A2(
		$elm$json$Json$Encode$encode,
		4,
		$the_sett$json_optional$Json$Encode$Optional$objectMaySkip(
			_List_fromArray(
				[
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('name', app.name)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('title', app.title)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('summary', app.summary)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('description', appDescription)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('dxapi', app.dxapi)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2('version', app.version)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$elm$json$Json$Encode$string,
					_Utils_Tuple2(
						'developerNotes',
						$author$project$Main$stringToMaybe(app.developerNotes))),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
					_Utils_Tuple2(
						'categories',
						$author$project$Main$listToMaybe(app.categories))),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
					_Utils_Tuple2(
						'developers',
						$author$project$Main$listToMaybe(app.developers))),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$elm$json$Json$Encode$list($elm$json$Json$Encode$string),
					_Utils_Tuple2(
						'authorizedUsers',
						$author$project$Main$listToMaybe(app.authorizedUsers))),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$author$project$Main$encodeRunSpec,
					_Utils_Tuple2('runSpec', app.runSpec)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$list($author$project$Main$encodeInputSpec),
					_Utils_Tuple2('inputSpec', app.inputSpec)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$field,
					$elm$json$Json$Encode$list($author$project$Main$encodeOutputSpec),
					_Utils_Tuple2('outputSpec', app.outputSpec)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$author$project$Main$encodeAccessSpec,
					_Utils_Tuple2('access', app.access)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					$author$project$Main$encodeHttpsApp,
					_Utils_Tuple2('httpsApp', app.httpsApp)),
					A2(
					$the_sett$json_optional$Json$Encode$Optional$optionalField,
					A2($elm$json$Json$Encode$dict, $elm$core$Basics$identity, $author$project$Main$encodeRegionalOptions),
					_Utils_Tuple2(
						'regionalOptions',
						$author$project$Main$dictToMaybe(app.regionalOptions)))
				])));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $Chadtech$unique_list$List$Unique$UniqueList = function (a) {
	return {$: 'UniqueList', a: a};
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $Chadtech$unique_list$List$Unique$consIfNotMember = F2(
	function (el, list) {
		return A2($elm$core$List$member, el, list) ? list : A2($elm$core$List$cons, el, list);
	});
var $Chadtech$unique_list$List$Unique$fromList = function (list) {
	return $Chadtech$unique_list$List$Unique$UniqueList(
		A3($elm$core$List$foldr, $Chadtech$unique_list$List$Unique$consIfNotMember, _List_Nil, list));
};
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{caseInsensitive: false, multiline: false},
		string);
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, idx, xs));
	});
var $author$project$Main$initialAccessSpec = {allProjects: $elm$core$Maybe$Nothing, developer: $elm$core$Maybe$Nothing, network: _List_Nil, project: $elm$core$Maybe$Nothing, projectCreation: $elm$core$Maybe$Nothing};
var $author$project$Main$initialClusterSpec = {bootstrapScript: '', initialInstanceCount: 1, ports: '', type_: 'generic', version: ''};
var $author$project$Main$validHttpsPorts = _List_fromArray(
	[443, 8080, 8081]);
var $author$project$Main$initialHttpsApp = {ports: $author$project$Main$validHttpsPorts, sharedAccess: 'NONE'};
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $elm$regex$Regex$never = _Regex_never;
var $author$project$Main$isSemanticVersion = function (val) {
	var pattern = A2(
		$elm$core$Maybe$withDefault,
		$elm$regex$Regex$never,
		$elm$regex$Regex$fromString('^[0-9]+[.][0-9]+[.][0-9]+$'));
	return $elm$core$List$length(
		A2($elm$regex$Regex$find, pattern, val)) === 1;
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$sortBy = _List_sortBy;
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $Chadtech$unique_list$List$Unique$toList = function (_v0) {
	var list = _v0.a;
	return list;
};
var $author$project$Main$mkSortedUniqList = F2(
	function (vals, newVal) {
		return $Chadtech$unique_list$List$Unique$toList(
			$Chadtech$unique_list$List$Unique$fromList(
				$elm$core$List$sort(
					_Utils_ap(
						vals,
						_List_fromArray(
							[newVal])))));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $elm_community$list_extra$List$Extra$removeAt = F2(
	function (index, l) {
		if (index < 0) {
			return l;
		} else {
			var _v0 = A2($elm$core$List$drop, index, l);
			if (!_v0.b) {
				return l;
			} else {
				var rest = _v0.b;
				return _Utils_ap(
					A2($elm$core$List$take, index, l),
					rest);
			}
		}
	});
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $elm$core$String$trim = _String_trim;
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm_community$list_extra$List$Extra$updateAt = F3(
	function (index, fn, list) {
		if (index < 0) {
			return list;
		} else {
			var tail = A2($elm$core$List$drop, index, list);
			var head = A2($elm$core$List$take, index, list);
			if (tail.b) {
				var x = tail.a;
				var xs = tail.b;
				return _Utils_ap(
					head,
					A2(
						$elm$core$List$cons,
						fn(x),
						xs));
			} else {
				return list;
			}
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'CloseExecDependsDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingExecDependsIndex: $elm$core$Maybe$Nothing, execDependsToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseInputSpecDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingInputSpecIndex: $elm$core$Maybe$Nothing, inputSpecToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseOutputSpecDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingOutputSpecIndex: $elm$core$Maybe$Nothing, outputSpecToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseRegionalOptionsDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseSysReqDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingSysReqHost: $elm$core$Maybe$Nothing, sysReqToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseTimeoutPolicyDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingTimeoutPolicyIndex: $elm$core$Maybe$Nothing, timeoutPolicyToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'CloseJsonDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{incomingJson: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteAccessSpecNetwork':
				var network = msg.a;
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								network: A2(
									$elm$core$List$filter,
									function (n) {
										return !_Utils_eq(n, network);
									},
									access.network)
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteAppCategory':
				var cat = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						categories: A2(
							$elm$core$List$filter,
							function (c) {
								return !_Utils_eq(c, cat);
							},
							app.categories)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteAuthorizedUser':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						authorizedUsers: A2(
							$elm$core$List$filter,
							function (u) {
								return !_Utils_eq(u, val);
							},
							app.authorizedUsers)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteDeveloper':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						developers: A2(
							$elm$core$List$filter,
							function (d) {
								return !_Utils_eq(d, val);
							},
							app.developers)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteExecDepends':
				var index = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{
						execDepends: A2($elm_community$list_extra$List$Extra$removeAt, index, runSpec.execDepends)
					});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteExecDependsStage':
				var stage = msg.a;
				var newExec = A2(
					$elm$core$Maybe$map,
					function (exec) {
						return _Utils_update(
							exec,
							{
								stages: A2(
									$elm$core$List$filter,
									function (s) {
										return !_Utils_eq(s, stage);
									},
									exec.stages)
							});
					},
					model.execDependsToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsToModify: newExec}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteHttpsAppPort':
				var index = msg.a;
				var app = model.app;
				var newHttpsApp = A2(
					$elm$core$Maybe$map,
					function (httpsApp) {
						return _Utils_update(
							httpsApp,
							{
								ports: A2($elm_community$list_extra$List$Extra$removeAt, index, httpsApp.ports)
							});
					},
					app.httpsApp);
				var newApp = _Utils_update(
					app,
					{httpsApp: newHttpsApp});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteInputSpec':
				var index = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						inputSpec: A2($elm_community$list_extra$List$Extra$removeAt, index, app.inputSpec)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteInputSpecChoice':
				var choice = msg.a;
				var newSpec = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								choices: A2(
									$elm$core$List$filter,
									function (c) {
										return !_Utils_eq(c, choice);
									},
									spec.choices)
							});
					},
					model.inputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newSpec}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteInputSpecPattern':
				var pattern = msg.a;
				var newSpec = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: A2(
									$elm$core$List$filter,
									function (p) {
										return !_Utils_eq(p, pattern);
									},
									spec.patterns)
							});
					},
					model.inputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newSpec}),
					$elm$core$Platform$Cmd$none);
			case 'DecodeIncomingJson':
				var newModel = $author$project$Main$decodeIncomingJson(model);
				var newJson = function () {
					var _v1 = newModel.jsonError;
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						return model.incomingJson;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						newModel,
						{incomingJson: newJson}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteOutputSpec':
				var index = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						outputSpec: A2($elm_community$list_extra$List$Extra$removeAt, index, app.outputSpec)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteOutputSpecPattern':
				var pattern = msg.a;
				var newSpec = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: A2(
									$elm$core$List$filter,
									function (p) {
										return !_Utils_eq(p, pattern);
									},
									spec.patterns)
							});
					},
					model.outputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{outputSpecToModify: newSpec}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteRegionalOptionsResource':
				var resource = msg.a;
				var newOpts = A2(
					$elm$core$Maybe$map,
					function (_v2) {
						var regionName = _v2.a;
						var opts = _v2.b;
						var curResources = function () {
							var _v3 = opts.resources;
							if (_v3.$ === 'ResourceString') {
								var s = _v3.a;
								return _List_fromArray(
									[s]);
							} else {
								var vals = _v3.a;
								return vals;
							}
						}();
						var newResources = A2(
							$elm$core$List$filter,
							function (r) {
								return !_Utils_eq(r, resource);
							},
							curResources);
						return _Utils_Tuple2(
							regionName,
							_Utils_update(
								opts,
								{
									resources: $author$project$Main$ResourceList(newResources)
								}));
					},
					model.regionalOptionsToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsToModify: newOpts}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteRegionalOptions':
				var regionName = msg.a;
				var app = model.app;
				var newOpts = $elm$core$Dict$fromList(
					A2(
						$elm$core$List$filter,
						function (_v4) {
							var regName = _v4.a;
							var opt = _v4.b;
							return !_Utils_eq(regName, regionName);
						},
						$elm$core$Dict$toList(app.regionalOptions)));
				var newApp = _Utils_update(
					app,
					{regionalOptions: newOpts});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteSysReq':
				var index = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newSysReqs = $elm$core$Dict$fromList(
					A2(
						$elm_community$list_extra$List$Extra$removeAt,
						index,
						$elm$core$Dict$toList(runSpec.systemRequirements)));
				var newRunSpec = _Utils_update(
					runSpec,
					{systemRequirements: newSysReqs});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'DeleteTimeoutPolicy':
				var index = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newTimeoutPolicy = $elm$core$Dict$fromList(
					A2(
						$elm_community$list_extra$List$Extra$removeAt,
						index,
						$elm$core$Dict$toList(runSpec.timeoutPolicy)));
				var newRunSpec = _Utils_update(
					runSpec,
					{timeoutPolicy: newTimeoutPolicy});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'EnableAccess':
				var enabled = msg.a;
				var newAccessSpec = enabled ? $elm$core$Maybe$Just($author$project$Main$initialAccessSpec) : $elm$core$Maybe$Nothing;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{access: newAccessSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, enableAccess: enabled}),
					$elm$core$Platform$Cmd$none);
			case 'EnableHttpsApp':
				var enabled = msg.a;
				var newHttpsApp = enabled ? $elm$core$Maybe$Just($author$project$Main$initialHttpsApp) : $elm$core$Maybe$Nothing;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{httpsApp: newHttpsApp});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, enableHttpsApp: enabled}),
					$elm$core$Platform$Cmd$none);
			case 'LinkClicked':
				var urlRequest = msg.a;
				if (urlRequest.$ === 'Internal') {
					var url = urlRequest.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.key,
							$elm$url$Url$toString(url)));
				} else {
					var href = urlRequest.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(href));
				}
			case 'ModifyExecDependsDialog':
				var newExecDepends = msg.a;
				var index = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingExecDependsIndex: index,
							execDependsToModify: $elm$core$Maybe$Just(newExecDepends)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ModifyInputSpecDialog':
				var newInputSpec = msg.a;
				var index = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingInputSpecIndex: index,
							inputSpecToModify: $elm$core$Maybe$Just(newInputSpec)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ModifyOutputSpecDialog':
				var newOutputSpec = msg.a;
				var index = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingOutputSpecIndex: index,
							outputSpecToModify: $elm$core$Maybe$Just(newOutputSpec)
						}),
					$elm$core$Platform$Cmd$none);
			case 'ModifyRegionalOptionsDialog':
				var regionName = msg.a;
				var newOpts = msg.b;
				var index = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							regionalOptionsToModify: $elm$core$Maybe$Just(
								_Utils_Tuple2(regionName, newOpts))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ModifySysReqDialog':
				var entryPointName = msg.a;
				var regionName = msg.b;
				var newSysReq = msg.c;
				var instanceType = newSysReq.instanceType;
				var newHost = A2($elm$core$String$contains, 'azure', instanceType) ? $author$project$Main$Azure : $author$project$Main$Aws;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingSysReqHost: $elm$core$Maybe$Just(newHost),
							sysReqToModify: $elm$core$Maybe$Just(
								_Utils_Tuple3(entryPointName, regionName, newSysReq))
						}),
					$elm$core$Platform$Cmd$none);
			case 'ModifyTimeoutPolicyDialog':
				var entryPointName = msg.a;
				var newTimeoutPolicy = msg.b;
				var index = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingTimeoutPolicyIndex: index,
							timeoutPolicyToModify: $elm$core$Maybe$Just(
								_Utils_Tuple2(entryPointName, newTimeoutPolicy))
						}),
					$elm$core$Platform$Cmd$none);
			case 'SaveExecDepends':
				var app = model.app;
				var runSpec = app.runSpec;
				var newExecDepends = function () {
					var _v6 = model.execDependsToModify;
					if (_v6.$ === 'Just') {
						var val = _v6.a;
						var _v7 = model.editingExecDependsIndex;
						if (_v7.$ === 'Just') {
							var i = _v7.a;
							return A3(
								$elm_community$list_extra$List$Extra$updateAt,
								i,
								function (_v8) {
									return val;
								},
								runSpec.execDepends);
						} else {
							return _Utils_ap(
								runSpec.execDepends,
								_List_fromArray(
									[val]));
						}
					} else {
						return runSpec.execDepends;
					}
				}();
				var newRunSpec = _Utils_update(
					runSpec,
					{execDepends: newExecDepends});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, editingExecDependsIndex: $elm$core$Maybe$Nothing, execDependsToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'SaveInputSpecDefault':
				var toBool = function (s) {
					return $elm$core$String$toLower(s) === 'true';
				};
				var curDefault = model.inputSpecToModifyDefault;
				var convertedDefault = function () {
					var _v12 = model.inputSpecToModify;
					if (_v12.$ === 'Just') {
						var spec = _v12.a;
						var _v13 = spec._class;
						switch (_v13.$) {
							case 'InputSpecBool':
								return $elm$core$Maybe$Just(
									$author$project$Main$DefaultValBool(
										toBool(curDefault)));
							case 'InputSpecInt':
								return A2(
									$elm$core$Maybe$map,
									function (i) {
										return $author$project$Main$DefaultValInt(i);
									},
									$elm$core$String$toInt(curDefault));
							case 'InputSpecFloat':
								return A2(
									$elm$core$Maybe$map,
									function (f) {
										return $author$project$Main$DefaultValFloat(f);
									},
									$elm$core$String$toFloat(curDefault));
							default:
								return $elm$core$Maybe$Just(
									$author$project$Main$DefaultValString(curDefault));
						}
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				var app = model.app;
				var _v9 = function () {
					var _v10 = model.inputSpecToModify;
					if (_v10.$ === 'Just') {
						var inputSpecToModify = _v10.a;
						if (convertedDefault.$ === 'Just') {
							var def = convertedDefault.a;
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(
									_Utils_update(
										inputSpecToModify,
										{_default: def})),
								$elm$core$Maybe$Nothing);
						} else {
							return _Utils_Tuple2(
								$elm$core$Maybe$Just(inputSpecToModify),
								$elm$core$Maybe$Just('Invalid default'));
						}
					} else {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					}
				}();
				var newInputSpec = _v9.a;
				var newErr = _v9.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpec, inputSpecToModifyError: newErr}),
					$elm$core$Platform$Cmd$none);
			case 'SaveInputSpec':
				var app = model.app;
				var newInputSpec = function () {
					var _v14 = model.inputSpecToModify;
					if (_v14.$ === 'Just') {
						var spec = _v14.a;
						var _v15 = model.editingInputSpecIndex;
						if (_v15.$ === 'Just') {
							var i = _v15.a;
							return A3(
								$elm_community$list_extra$List$Extra$updateAt,
								i,
								function (_v16) {
									return spec;
								},
								app.inputSpec);
						} else {
							return _Utils_ap(
								app.inputSpec,
								_List_fromArray(
									[spec]));
						}
					} else {
						return app.inputSpec;
					}
				}();
				var newApp = _Utils_update(
					app,
					{inputSpec: newInputSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, editingInputSpecIndex: $elm$core$Maybe$Nothing, inputSpecToModify: $elm$core$Maybe$Nothing, inputSpecToModifyDefault: '', inputSpecToModifyError: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'SaveOutputSpec':
				var app = model.app;
				var newOutputSpec = function () {
					var _v17 = model.outputSpecToModify;
					if (_v17.$ === 'Just') {
						var spec = _v17.a;
						var _v18 = model.editingOutputSpecIndex;
						if (_v18.$ === 'Just') {
							var i = _v18.a;
							return A3(
								$elm_community$list_extra$List$Extra$updateAt,
								i,
								function (_v19) {
									return spec;
								},
								app.outputSpec);
						} else {
							return _Utils_ap(
								app.outputSpec,
								_List_fromArray(
									[spec]));
						}
					} else {
						return app.outputSpec;
					}
				}();
				var newApp = _Utils_update(
					app,
					{outputSpec: newOutputSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, editingOutputSpecIndex: $elm$core$Maybe$Nothing, outputSpecToModify: $elm$core$Maybe$Nothing, outputSpecToModifyError: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'SaveRegionalOptions':
				var app = model.app;
				var curOpts = app.regionalOptions;
				var _v20 = function () {
					var _v21 = model.regionalOptionsToModify;
					if (_v21.$ === 'Just') {
						var _v22 = _v21.a;
						var regionName = _v22.a;
						var opt = _v22.b;
						return _Utils_Tuple2(
							A3(
								$elm$core$Dict$update,
								regionName,
								function (_v23) {
									return $elm$core$Maybe$Just(opt);
								},
								curOpts),
							$elm$core$Maybe$Nothing);
					} else {
						return _Utils_Tuple2(curOpts, model.regionalOptionsToModify);
					}
				}();
				var newOpts = _v20.a;
				var editingOpt = _v20.b;
				var newApp = _Utils_update(
					app,
					{regionalOptions: newOpts});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, regionalOptionsToModify: editingOpt}),
					$elm$core$Platform$Cmd$none);
			case 'SaveSysReqToRunSpec':
				var app = model.app;
				var runSpec = app.runSpec;
				var sysReqs = runSpec.systemRequirements;
				var _v24 = function () {
					var _v25 = model.sysReqToModify;
					if (_v25.$ === 'Just') {
						var _v26 = _v25.a;
						var entryPointName = _v26.a;
						var req = _v26.c;
						return _Utils_Tuple2(
							A3(
								$elm$core$Dict$update,
								entryPointName,
								function (_v27) {
									return $elm$core$Maybe$Just(req);
								},
								sysReqs),
							$elm$core$Maybe$Nothing);
					} else {
						return _Utils_Tuple2(sysReqs, model.sysReqToModify);
					}
				}();
				var newSysReq = _v24.a;
				var newEditingSysReq = _v24.b;
				var newRunSpec = _Utils_update(
					runSpec,
					{systemRequirements: newSysReq});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, sysReqToModify: newEditingSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'SaveSysReqToRegionalOptions':
				var regionName = msg.a;
				var app = model.app;
				var _v28 = function () {
					var _v29 = model.sysReqToModify;
					if (_v29.$ === 'Just') {
						var _v30 = _v29.a;
						var entryPointName = _v30.a;
						var editingSysReq = _v30.c;
						var _v31 = A2($elm$core$Dict$get, regionName, app.regionalOptions);
						if (_v31.$ === 'Just') {
							var regionalOptions = _v31.a;
							var newSysReqs = A3(
								$elm$core$Dict$update,
								entryPointName,
								function (_v33) {
									return $elm$core$Maybe$Just(editingSysReq);
								},
								regionalOptions.systemRequirements);
							var newOpts = _Utils_update(
								regionalOptions,
								{systemRequirements: newSysReqs});
							var newRegOptsDict = A3(
								$elm$core$Dict$update,
								regionName,
								function (_v32) {
									return $elm$core$Maybe$Just(newOpts);
								},
								app.regionalOptions);
							return _Utils_Tuple2(newRegOptsDict, $elm$core$Maybe$Nothing);
						} else {
							return _Utils_Tuple2(app.regionalOptions, model.sysReqToModify);
						}
					} else {
						return _Utils_Tuple2(app.regionalOptions, model.sysReqToModify);
					}
				}();
				var newRegionalOptions = _v28.a;
				var newEditingSysReq = _v28.b;
				var newApp = _Utils_update(
					app,
					{regionalOptions: newRegionalOptions});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, sysReqToModify: newEditingSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'SaveTimeoutPolicy':
				var app = model.app;
				var runSpec = app.runSpec;
				var timeoutPolicy = $elm$core$Dict$toList(runSpec.timeoutPolicy);
				var newTimeoutPolicy = function () {
					var _v34 = model.timeoutPolicyToModify;
					if (_v34.$ === 'Just') {
						var _v35 = _v34.a;
						var entryPointName = _v35.a;
						var policy = _v35.b;
						var _v36 = model.editingTimeoutPolicyIndex;
						if (_v36.$ === 'Just') {
							var i = _v36.a;
							return A3(
								$elm_community$list_extra$List$Extra$updateAt,
								i,
								function (_v37) {
									return _Utils_Tuple2(entryPointName, policy);
								},
								timeoutPolicy);
						} else {
							return _Utils_ap(
								timeoutPolicy,
								_List_fromArray(
									[
										_Utils_Tuple2(entryPointName, policy)
									]));
						}
					} else {
						return timeoutPolicy;
					}
				}();
				var newRunSpec = _Utils_update(
					runSpec,
					{
						timeoutPolicy: $elm$core$Dict$fromList(newTimeoutPolicy)
					});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, editingTimeoutPolicyIndex: $elm$core$Maybe$Nothing, timeoutPolicyToModify: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'SetExecDependsToModify':
				var index = msg.a;
				var _v38 = function () {
					var _v39 = A2($elm_community$list_extra$List$Extra$getAt, index, model.app.runSpec.execDepends);
					if (_v39.$ === 'Just') {
						var val = _v39.a;
						return _Utils_Tuple2(
							$elm$core$Maybe$Just(val),
							$elm$core$Maybe$Just(index));
					} else {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					}
				}();
				var newExecDepends = _v38.a;
				var indexValue = _v38.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingExecDependsIndex: indexValue, execDependsToModify: newExecDepends}),
					$elm$core$Platform$Cmd$none);
			case 'SetInputSpecToModify':
				var index = msg.a;
				var _v40 = function () {
					var _v41 = A2($elm_community$list_extra$List$Extra$getAt, index, model.app.inputSpec);
					if (_v41.$ === 'Just') {
						var val = _v41.a;
						return _Utils_Tuple2(
							$elm$core$Maybe$Just(val),
							$elm$core$Maybe$Just(index));
					} else {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					}
				}();
				var newInputSpec = _v40.a;
				var indexValue = _v40.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingInputSpecIndex: indexValue, inputSpecToModify: newInputSpec}),
					$elm$core$Platform$Cmd$none);
			case 'SetOutputSpecToModify':
				var index = msg.a;
				var _v42 = function () {
					var _v43 = A2($elm_community$list_extra$List$Extra$getAt, index, model.app.outputSpec);
					if (_v43.$ === 'Just') {
						var val = _v43.a;
						return _Utils_Tuple2(
							$elm$core$Maybe$Just(val),
							$elm$core$Maybe$Just(index));
					} else {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					}
				}();
				var newOutputSpec = _v42.a;
				var indexValue = _v42.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingOutputSpecIndex: indexValue, outputSpecToModify: newOutputSpec}),
					$elm$core$Platform$Cmd$none);
			case 'SetRegionalOptionsToModify':
				var regionName = msg.a;
				var optToModify = A2(
					$elm$core$Maybe$map,
					function (opt) {
						return _Utils_Tuple2(regionName, opt);
					},
					A2($elm$core$Dict$get, regionName, model.app.regionalOptions));
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsToModify: optToModify}),
					$elm$core$Platform$Cmd$none);
			case 'SetRegionalOptionsSysReqToModify':
				var regionName = msg.a;
				var entryPointName = msg.b;
				var sysReq = msg.c;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sysReqToModify: $elm$core$Maybe$Just(
								_Utils_Tuple3(
									entryPointName,
									$elm$core$Maybe$Just(regionName),
									sysReq))
						}),
					$elm$core$Platform$Cmd$none);
			case 'SetRunSpecSysReqToModify':
				var entryPointName = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							sysReqToModify: A2(
								$elm$core$Maybe$map,
								function (val) {
									return _Utils_Tuple3(entryPointName, $elm$core$Maybe$Nothing, val);
								},
								A2($elm$core$Dict$get, entryPointName, model.app.runSpec.systemRequirements))
						}),
					$elm$core$Platform$Cmd$none);
			case 'SetTimeoutPolicyToModify':
				var index = msg.a;
				var policies = $elm$core$Dict$toList(model.app.runSpec.timeoutPolicy);
				var _v44 = function () {
					var _v45 = A2($elm_community$list_extra$List$Extra$getAt, index, policies);
					if (_v45.$ === 'Just') {
						var val = _v45.a;
						return _Utils_Tuple2(
							$elm$core$Maybe$Just(val),
							$elm$core$Maybe$Just(index));
					} else {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					}
				}();
				var newTimeoutPolicy = _v44.a;
				var indexValue = _v44.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{editingTimeoutPolicyIndex: indexValue, timeoutPolicyToModify: newTimeoutPolicy}),
					$elm$core$Platform$Cmd$none);
			case 'ShowJsonDialog':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							incomingJson: $elm$core$Maybe$Just(
								$author$project$Main$encodeApp(model.app))
						}),
					$elm$core$Platform$Cmd$none);
			case 'TabMsg':
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{tabState: state}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecDeveloper':
				var val = msg.a;
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								developer: $elm$core$Maybe$Just(val)
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecProjectCreation':
				var val = msg.a;
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								projectCreation: $elm$core$Maybe$Just(val)
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecOptional':
				var val = msg.a;
				var newInputSpecToModify = function () {
					var _v46 = model.inputSpecToModify;
					if (_v46.$ === 'Just') {
						var spec = _v46.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								spec,
								{optional: val}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateCustomCategory':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{customCategory: val}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAuthorizedUser':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							authorizedUser: $elm$core$String$trim(val)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateDeveloper':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							developer: $elm$core$String$trim(val)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAddAuthorizedUser':
				var app = model.app;
				var newAuthorizedUsers = $elm$core$String$isEmpty(model.authorizedUser) ? app.authorizedUsers : A2($author$project$Main$mkSortedUniqList, app.authorizedUsers, model.authorizedUser);
				var newApp = _Utils_update(
					app,
					{authorizedUsers: newAuthorizedUsers});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, authorizedUser: ''}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAddDeveloper':
				var app = model.app;
				var newDevelopers = $elm$core$String$isEmpty(model.developer) ? app.developers : A2($author$project$Main$mkSortedUniqList, app.developers, model.developer);
				var newApp = _Utils_update(
					app,
					{developers: newDevelopers});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, developer: ''}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppAddCustomCategory':
				var customCat = $elm$core$String$trim(model.customCategory);
				var app = model.app;
				var _v47 = function () {
					var _v48 = $elm$core$String$isEmpty(customCat);
					if (_v48) {
						return _Utils_Tuple2(customCat, app.categories);
					} else {
						return _Utils_Tuple2(
							'',
							A2($author$project$Main$mkSortedUniqList, app.categories, customCat));
					}
				}();
				var newCustomCat = _v47.a;
				var newCats = _v47.b;
				var newApp = _Utils_update(
					app,
					{categories: newCats});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, customCategory: newCustomCat}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecNetwork':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{accessSpecNetwork: val}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecAllProjects':
				var val = msg.a;
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								allProjects: $author$project$Main$stringToMaybe(
									$elm$core$String$trim(val))
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecProject':
				var val = msg.a;
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								project: $author$project$Main$stringToMaybe(
									$elm$core$String$trim(val))
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAccessSpecAddNetwork':
				var app = model.app;
				var newAccess = A2(
					$elm$core$Maybe$map,
					function (access) {
						return _Utils_update(
							access,
							{
								network: _Utils_ap(
									$elm$core$List$sort(access.network),
									_List_fromArray(
										[model.accessSpecNetwork]))
							});
					},
					app.access);
				var newApp = _Utils_update(
					app,
					{access: newAccess});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{accessSpecNetwork: '', app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppCategories':
				var newCat = msg.a;
				var app = model.app;
				var newCats = function () {
					var _v49 = A2($elm$core$String$startsWith, '--', newCat);
					if (_v49) {
						return app.categories;
					} else {
						return A2($author$project$Main$mkSortedUniqList, app.categories, newCat);
					}
				}();
				var newApp = _Utils_update(
					app,
					{categories: newCats});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppDescription':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{description: val});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppDxapi':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{dxapi: val});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppName':
				var val = msg.a;
				var newVal = A3(
					$elm$core$String$replace,
					' ',
					'_',
					$elm$core$String$toLower(val));
				var badChars = A2(
					$elm$core$Maybe$withDefault,
					$elm$regex$Regex$never,
					$elm$regex$Regex$fromString('[^a-z0-9_]'));
				var app = model.app;
				var _v50 = function () {
					var _v51 = A2($elm$regex$Regex$contains, badChars, newVal);
					if (_v51) {
						return _Utils_Tuple2(
							app.name,
							$elm$core$Maybe$Just('Disallowed character in name'));
					} else {
						return _Utils_Tuple2(newVal, $elm$core$Maybe$Nothing);
					}
				}();
				var newName = _v50.a;
				var err = _v50.b;
				var newApp = _Utils_update(
					app,
					{name: newName});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, error: err}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppDeveloperNotes':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{
						developerNotes: $elm$core$String$trim(val)
					});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppSummary':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{summary: val});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppTitle':
				var val = msg.a;
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{title: val});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateAppVersion':
				var val = msg.a;
				var error = function () {
					var _v52 = $author$project$Main$isSemanticVersion(val);
					if (_v52) {
						return $elm$core$Maybe$Nothing;
					} else {
						return $elm$core$Maybe$Just('Not a semantic version');
					}
				}();
				var app = model.app;
				var newApp = _Utils_update(
					app,
					{version: val});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, error: error}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExecDependsStage':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsStage: val}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExecDependsAddStage':
				var newExec = A2(
					$elm$core$Maybe$map,
					function (exec) {
						return _Utils_update(
							exec,
							{
								stages: $elm$core$List$sort(
									_Utils_ap(
										exec.stages,
										_List_fromArray(
											[model.execDependsStage])))
							});
					},
					model.execDependsToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsStage: '', execDependsToModify: newExec}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExecDependsName':
				var val = msg.a;
				var newExecDependsToModify = function () {
					var _v53 = model.execDependsToModify;
					if (_v53.$ === 'Just') {
						var execDependsToModify = _v53.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								execDependsToModify,
								{name: val}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsToModify: newExecDependsToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExecDependsPackageManager':
				var val = msg.a;
				var newPackageManager = function () {
					switch (val) {
						case 'cpan':
							return $author$project$Main$Cpan;
						case 'cran':
							return $author$project$Main$Cran;
						case 'gem':
							return $author$project$Main$Gem;
						case 'pip':
							return $author$project$Main$Pip;
						default:
							return $author$project$Main$Apt;
					}
				}();
				var newExecDependsToModify = function () {
					var _v54 = model.execDependsToModify;
					if (_v54.$ === 'Just') {
						var execDependsToModify = _v54.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								execDependsToModify,
								{packageManager: newPackageManager}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsToModify: newExecDependsToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateExecDependsVersion':
				var val = msg.a;
				var newExecDependsToModify = function () {
					var _v56 = model.execDependsToModify;
					if (_v56.$ === 'Just') {
						var execDependsToModify = _v56.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								execDependsToModify,
								{version: val}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{execDependsToModify: newExecDependsToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateHttpsAppAddPort':
				var val = msg.a;
				var newPorts = function (httpsApp) {
					var _v57 = $elm$core$String$toInt(val);
					if (_v57.$ === 'Just') {
						var newPort = _v57.a;
						return A2($author$project$Main$mkSortedUniqList, httpsApp.ports, newPort);
					} else {
						return httpsApp.ports;
					}
				};
				var app = model.app;
				var newHttpsApp = A2(
					$elm$core$Maybe$map,
					function (httpsApp) {
						return _Utils_update(
							httpsApp,
							{
								ports: newPorts(httpsApp)
							});
					},
					app.httpsApp);
				var newApp = _Utils_update(
					app,
					{httpsApp: newHttpsApp});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp, httpsAppPort: $elm$core$Maybe$Nothing}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateHttpsAppSharedAccess':
				var val = msg.a;
				var app = model.app;
				var newHttpsApp = A2(
					$elm$core$Maybe$map,
					function (httpsApp) {
						return _Utils_update(
							httpsApp,
							{sharedAccess: val});
					},
					app.httpsApp);
				var newApp = _Utils_update(
					app,
					{httpsApp: newHttpsApp});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecName':
				var val = msg.a;
				var notAllowed = A2(
					$elm$core$Maybe$withDefault,
					$elm$regex$Regex$never,
					$elm$regex$Regex$fromString('[^0-9a-zA-Z_]'));
				var newName = A3($elm$core$String$replace, ' ', '_', val);
				var cleaned = A3(
					$elm$regex$Regex$replace,
					notAllowed,
					function (_v59) {
						return '';
					},
					newName);
				var newInputSpecToModify = function () {
					var _v58 = model.inputSpecToModify;
					if (_v58.$ === 'Just') {
						var spec = _v58.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								spec,
								{name: cleaned}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecClass':
				var val = msg.a;
				var _v60 = function () {
					switch (val) {
						case 'applet':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecApplet,
								$author$project$Main$DefaultValString(''));
						case 'array:applet':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayApplet,
								$author$project$Main$DefaultValString(''));
						case 'array:boolean':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayBoolean,
								$author$project$Main$DefaultValString(''));
						case 'array:file':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayFile,
								$author$project$Main$DefaultValString(''));
						case 'array:float':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayFloat,
								$author$project$Main$DefaultValString(''));
						case 'array:int':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayInt,
								$author$project$Main$DefaultValString(''));
						case 'array:record':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayRecord,
								$author$project$Main$DefaultValString(''));
						case 'array:string':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecArrayString,
								$author$project$Main$DefaultValString(''));
						case 'boolean':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecBool,
								$author$project$Main$DefaultValBool(false));
						case 'file':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecFile,
								$author$project$Main$DefaultValString(''));
						case 'float':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecFloat,
								$author$project$Main$DefaultValFloat(0.0));
						case 'hash':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecHash,
								$author$project$Main$DefaultValString(''));
						case 'int':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecInt,
								$author$project$Main$DefaultValInt(0));
						case 'record':
							return _Utils_Tuple2(
								$author$project$Main$InputSpecRecord,
								$author$project$Main$DefaultValString(''));
						default:
							return _Utils_Tuple2(
								$author$project$Main$InputSpecString,
								$author$project$Main$DefaultValString(''));
					}
				}();
				var newType = _v60.a;
				var newDefaultValue = _v60.b;
				var newInputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{_class: newType, _default: newDefaultValue});
					},
					model.inputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							inputSpecToModify: newInputSpecToModify,
							inputSpecToModifyDefault: $author$project$Main$defaultValueToString(newDefaultValue)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecDefault':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModifyDefault: val}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecHelp':
				var val = msg.a;
				var newInputSpecToModify = function () {
					var _v62 = model.inputSpecToModify;
					if (_v62.$ === 'Just') {
						var spec = _v62.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								spec,
								{help: val}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecChoice':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							inputSpecChoice: $elm$core$String$trim(val)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecCustomPattern':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							inputSpecPattern: $elm$core$String$trim(val)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecAddChoice':
				var newChoice = model.inputSpecChoice;
				var newInputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								choices: $Chadtech$unique_list$List$Unique$toList(
									$Chadtech$unique_list$List$Unique$fromList(
										$elm$core$List$sort(
											_Utils_ap(
												spec.choices,
												_List_fromArray(
													[newChoice])))))
							});
					},
					model.inputSpecToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecChoice: '', inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecAddCustomPattern':
				var newPat = model.inputSpecPattern;
				var newInputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: $Chadtech$unique_list$List$Unique$toList(
									$Chadtech$unique_list$List$Unique$fromList(
										$elm$core$List$sort(
											_Utils_ap(
												spec.patterns,
												_List_fromArray(
													[newPat])))))
							});
					},
					model.inputSpecToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecPattern: '', inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecAddPattern':
				var newPat = msg.a;
				var newInputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: $Chadtech$unique_list$List$Unique$toList(
									$Chadtech$unique_list$List$Unique$fromList(
										$elm$core$List$sort(
											_Utils_ap(
												spec.patterns,
												_List_fromArray(
													[newPat])))))
							});
					},
					model.inputSpecToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateInputSpecLabel':
				var val = msg.a;
				var newInputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{label: val});
					},
					model.inputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{inputSpecToModify: newInputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateIncomingJson':
				var json = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							incomingJson: $elm$core$Maybe$Just(json)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateOutputSpecCustomPattern':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							outputSpecPattern: $elm$core$String$trim(val)
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateOutputSpecAddPattern':
				var newPat = msg.a;
				var newOutputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: $elm$core$List$sort(
									_Utils_ap(
										spec.patterns,
										_List_fromArray(
											[newPat])))
							});
					},
					model.outputSpecToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{outputSpecToModify: newOutputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateOutputSpecAddCustomPattern':
				var newPat = model.outputSpecPattern;
				var newOutputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{
								patterns: $elm$core$List$sort(
									_Utils_ap(
										spec.patterns,
										_List_fromArray(
											[newPat])))
							});
					},
					model.outputSpecToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{outputSpecPattern: '', outputSpecToModify: newOutputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateOutputSpecClass':
				var val = msg.a;
				var newOutputSpecToModify = A2(
					$elm$core$Maybe$map,
					function (spec) {
						return _Utils_update(
							spec,
							{_class: val});
					},
					model.outputSpecToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{outputSpecToModify: newOutputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateOutputSpecName':
				var val = msg.a;
				var notAllowed = A2(
					$elm$core$Maybe$withDefault,
					$elm$regex$Regex$never,
					$elm$regex$Regex$fromString('[^0-9a-zA-Z_]'));
				var newName = A3($elm$core$String$replace, ' ', '_', val);
				var cleaned = A3(
					$elm$regex$Regex$replace,
					notAllowed,
					function (_v64) {
						return '';
					},
					newName);
				var newOutputSpecToModify = function () {
					var _v63 = model.outputSpecToModify;
					if (_v63.$ === 'Just') {
						var spec = _v63.a;
						return $elm$core$Maybe$Just(
							_Utils_update(
								spec,
								{name: cleaned}));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}();
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{outputSpecToModify: newOutputSpecToModify}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRegionalOptionsRegionName':
				var val = msg.a;
				var newOpts = A2(
					$elm$core$Maybe$map,
					function (_v65) {
						var regionName = _v65.a;
						var opts = _v65.b;
						return _Utils_Tuple2(val, opts);
					},
					model.regionalOptionsToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsToModify: newOpts}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRegionalOptionsResource':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsResource: val}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRegionalOptionsAddResource':
				var newOpts = A2(
					$elm$core$Maybe$map,
					function (_v66) {
						var regionName = _v66.a;
						var opts = _v66.b;
						var curResources = function () {
							var _v67 = opts.resources;
							if (_v67.$ === 'ResourceString') {
								var s = _v67.a;
								return _List_fromArray(
									[s]);
							} else {
								var vals = _v67.a;
								return vals;
							}
						}();
						var newResources = $author$project$Main$ResourceList(
							_Utils_ap(
								curResources,
								_List_fromArray(
									[model.regionalOptionsResource])));
						return _Utils_Tuple2(
							regionName,
							_Utils_update(
								opts,
								{resources: newResources}));
					},
					model.regionalOptionsToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{regionalOptionsResource: '', regionalOptionsToModify: newOpts}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecInterpreter':
				var val = msg.a;
				var newInterpreter = function () {
					switch (val) {
						case 'python2.7':
							return $author$project$Main$Python27;
						case 'python3':
							return $author$project$Main$Python3;
						default:
							return $author$project$Main$Bash;
					}
				}();
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{interpreter: newInterpreter});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecDistribution':
				var val = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{distribution: val});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecRelease':
				var val = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{interpreter: $author$project$Main$Bash, release: val});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecRestartableEntryPoints':
				var val = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{restartableEntryPoints: val});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecVersion':
				var val = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{version: val});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateRunSpecFile':
				var val = msg.a;
				var app = model.app;
				var runSpec = app.runSpec;
				var newRunSpec = _Utils_update(
					runSpec,
					{file: val});
				var newApp = _Utils_update(
					app,
					{runSpec: newRunSpec});
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{app: newApp}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqMinGpus':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingSysReqMinGpus: A2(
								$elm$core$Maybe$withDefault,
								0,
								$elm$core$String$toInt(val))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqMinCores':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingSysReqMinCores: A2(
								$elm$core$Maybe$withDefault,
								0,
								$elm$core$String$toInt(val))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqMinMemory':
				var val = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingSysReqMinMemory: A2(
								$elm$core$Maybe$withDefault,
								0,
								$elm$core$String$toInt(val))
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqHost':
				var val = msg.a;
				var newHost = function () {
					if (val === 'Azure') {
						return $author$project$Main$Azure;
					} else {
						return $author$project$Main$Aws;
					}
				}();
				var newInstanceType = function () {
					if (newHost.$ === 'Aws') {
						return 'mem2_hdd2_x2';
					} else {
						return 'azure:mem1_ssd1_x2';
					}
				}();
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v69) {
						var entryPointName = _v69.a;
						var regionName = _v69.b;
						var sysReq = _v69.c;
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{instanceType: newInstanceType}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							editingSysReqHost: $elm$core$Maybe$Just(newHost),
							sysReqToModify: newSysReq
						}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqEnableCluster':
				var enabled = msg.a;
				var newClusterSpec = enabled ? $elm$core$Maybe$Just($author$project$Main$initialClusterSpec) : $elm$core$Maybe$Nothing;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v72) {
						var entryPointName = _v72.a;
						var regionName = _v72.b;
						var spec = _v72.c;
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								spec,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				var app = model.app;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{enableSysReqClusterDefinition: enabled, sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqEntryPointName':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v73) {
						var entryPointName = _v73.a;
						var regionName = _v73.b;
						var sysReq = _v73.c;
						return _Utils_Tuple3(val, regionName, sysReq);
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqInstanceType':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v74) {
						var entryPointName = _v74.a;
						var regionName = _v74.b;
						var sysReq = _v74.c;
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{instanceType: val}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqClusterSpecType':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v75) {
						var entryPointName = _v75.a;
						var regionName = _v75.b;
						var sysReq = _v75.c;
						var newClusterSpec = A2(
							$elm$core$Maybe$map,
							function (spec) {
								return _Utils_update(
									spec,
									{type_: val, version: ''});
							},
							sysReq.clusterSpec);
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqClusterSpecInitialInstanceCount':
				var val = msg.a;
				var newCount = A2(
					$elm$core$Maybe$withDefault,
					1,
					$elm$core$String$toInt(val));
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v76) {
						var entryPointName = _v76.a;
						var regionName = _v76.b;
						var sysReq = _v76.c;
						var newClusterSpec = A2(
							$elm$core$Maybe$map,
							function (spec) {
								return _Utils_update(
									spec,
									{initialInstanceCount: newCount});
							},
							sysReq.clusterSpec);
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqClusterSpecBootstrapScript':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v77) {
						var entryPointName = _v77.a;
						var regionName = _v77.b;
						var sysReq = _v77.c;
						var newClusterSpec = A2(
							$elm$core$Maybe$map,
							function (spec) {
								return _Utils_update(
									spec,
									{bootstrapScript: val});
							},
							sysReq.clusterSpec);
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqClusterSpecPorts':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v78) {
						var entryPointName = _v78.a;
						var regionName = _v78.b;
						var sysReq = _v78.c;
						var newClusterSpec = A2(
							$elm$core$Maybe$map,
							function (spec) {
								return _Utils_update(
									spec,
									{ports: val});
							},
							sysReq.clusterSpec);
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateSysReqClusterSpecVersion':
				var val = msg.a;
				var newSysReq = A2(
					$elm$core$Maybe$map,
					function (_v79) {
						var entryPointName = _v79.a;
						var regionName = _v79.b;
						var sysReq = _v79.c;
						var newClusterSpec = A2(
							$elm$core$Maybe$map,
							function (spec) {
								return _Utils_update(
									spec,
									{version: val});
							},
							sysReq.clusterSpec);
						return _Utils_Tuple3(
							entryPointName,
							regionName,
							_Utils_update(
								sysReq,
								{clusterSpec: newClusterSpec}));
					},
					model.sysReqToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{sysReqToModify: newSysReq}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateTimeoutPolicyEntryPointName':
				var val = msg.a;
				var newTimeout = A2(
					$elm$core$Maybe$map,
					function (_v80) {
						var entryPointName = _v80.a;
						var policy = _v80.b;
						return _Utils_Tuple2(val, policy);
					},
					model.timeoutPolicyToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{timeoutPolicyToModify: newTimeout}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateTimeoutPolicyHours':
				var val = msg.a;
				var trimmed = $elm$core$String$trim(val);
				var newVal = $elm$core$String$isEmpty(trimmed) ? '0' : trimmed;
				var newTimeout = A2(
					$elm$core$Maybe$map,
					function (_v81) {
						var entryPointName = _v81.a;
						var timeout = _v81.b;
						var newHours = A2(
							$elm$core$Maybe$withDefault,
							timeout.hours,
							$elm$core$String$toInt(newVal));
						return _Utils_Tuple2(
							entryPointName,
							_Utils_update(
								timeout,
								{hours: newHours}));
					},
					model.timeoutPolicyToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{timeoutPolicyToModify: newTimeout}),
					$elm$core$Platform$Cmd$none);
			case 'UpdateTimeoutPolicyMinutes':
				var val = msg.a;
				var trimmed = $elm$core$String$trim(val);
				var newVal = $elm$core$String$isEmpty(trimmed) ? '0' : trimmed;
				var newTimeout = A2(
					$elm$core$Maybe$map,
					function (_v82) {
						var entryPointName = _v82.a;
						var timeout = _v82.b;
						var newMinutes = A2(
							$elm$core$Maybe$withDefault,
							timeout.minutes,
							$elm$core$String$toInt(newVal));
						return _Utils_Tuple2(
							entryPointName,
							_Utils_update(
								timeout,
								{minutes: newMinutes}));
					},
					model.timeoutPolicyToModify);
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{timeoutPolicyToModify: newTimeout}),
					$elm$core$Platform$Cmd$none);
			default:
				var url = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{url: url}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $rundis$elm_bootstrap$Bootstrap$Tab$Config = function (a) {
	return {$: 'Config', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Tab$config = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Tab$Config(
		{attributes: _List_Nil, isPill: false, items: _List_Nil, layout: $elm$core$Maybe$Nothing, toMsg: toMsg, useHash: false, withAnimation: false});
};
var $elm$html$Html$div = _VirtualDom_node('div');
var $rundis$elm_bootstrap$Bootstrap$Grid$container = F2(
	function (attributes, children) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('container')
					]),
				attributes),
			children);
	});
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $rundis$elm_bootstrap$Bootstrap$Tab$Item = function (a) {
	return {$: 'Item', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Tab$item = function (rec) {
	return $rundis$elm_bootstrap$Bootstrap$Tab$Item(
		{id: rec.id, link: rec.link, pane: rec.pane});
};
var $rundis$elm_bootstrap$Bootstrap$Tab$items = F2(
	function (items_, _v0) {
		var configRec = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Tab$Config(
			_Utils_update(
				configRec,
				{items: items_}));
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$Link = function (a) {
	return {$: 'Link', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Tab$link = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Tab$Link(
			{attributes: attributes, children: children});
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$Pane = function (a) {
	return {$: 'Pane', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Tab$pane = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Tab$Pane(
			{attributes: attributes, children: children});
	});
var $author$project$Main$DeleteAccessSpecNetwork = function (a) {
	return {$: 'DeleteAccessSpecNetwork', a: a};
};
var $author$project$Main$EnableAccess = function (a) {
	return {$: 'EnableAccess', a: a};
};
var $author$project$Main$UpdateAccessSpecAddNetwork = {$: 'UpdateAccessSpecAddNetwork'};
var $author$project$Main$UpdateAccessSpecAllProjects = function (a) {
	return {$: 'UpdateAccessSpecAllProjects', a: a};
};
var $author$project$Main$UpdateAccessSpecDeveloper = function (a) {
	return {$: 'UpdateAccessSpecDeveloper', a: a};
};
var $author$project$Main$UpdateAccessSpecNetwork = function (a) {
	return {$: 'UpdateAccessSpecNetwork', a: a};
};
var $author$project$Main$UpdateAccessSpecProject = function (a) {
	return {$: 'UpdateAccessSpecProject', a: a};
};
var $author$project$Main$UpdateAccessSpecProjectCreation = function (a) {
	return {$: 'UpdateAccessSpecProjectCreation', a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size)
					});
			case 'Coloring':
				var coloring = modifier.a;
				return _Utils_update(
					options,
					{
						coloring: $elm$core$Maybe$Just(coloring)
					});
			case 'Block':
				return _Utils_update(
					options,
					{block: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions = {attributes: _List_Nil, block: false, coloring: $elm$core$Maybe$Nothing, disabled: false, size: $elm$core$Maybe$Nothing};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass = function (role) {
	switch (role.$) {
		case 'Primary':
			return 'primary';
		case 'Secondary':
			return 'secondary';
		case 'Success':
			return 'success';
		case 'Info':
			return 'info';
		case 'Warning':
			return 'warning';
		case 'Danger':
			return 'danger';
		case 'Dark':
			return 'dark';
		case 'Light':
			return 'light';
		default:
			return 'link';
	}
};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size.$) {
		case 'XS':
			return $elm$core$Maybe$Nothing;
		case 'SM':
			return $elm$core$Maybe$Just('sm');
		case 'MD':
			return $elm$core$Maybe$Just('md');
		case 'LG':
			return $elm$core$Maybe$Just('lg');
		default:
			return $elm$core$Maybe$Just('xl');
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier, $rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('btn', true),
						_Utils_Tuple2('btn-block', options.block),
						_Utils_Tuple2('disabled', options.disabled)
					])),
				$elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			function () {
				var _v0 = A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.size);
				if (_v0.$ === 'Just') {
					var s = _v0.a;
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$class('btn-' + s)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _v1 = options.coloring;
					if (_v1.$ === 'Just') {
						if (_v1.a.$ === 'Roled') {
							var role = _v1.a.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									'btn-' + $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						} else {
							var role = _v1.a.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									'btn-outline-' + $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						}
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes)));
};
var $rundis$elm_bootstrap$Bootstrap$Button$button = F2(
	function (options, children) {
		return A2(
			$elm$html$Html$button,
			$rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options),
			children);
	});
var $elm$html$Html$form = _VirtualDom_node('form');
var $rundis$elm_bootstrap$Bootstrap$Form$form = F2(
	function (attributes, children) {
		return A2($elm$html$Html$form, attributes, children);
	});
var $elm$html$Html$li = _VirtualDom_node('li');
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring = function (a) {
	return {$: 'Coloring', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Light = {$: 'Light'};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$light = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled($rundis$elm_bootstrap$Bootstrap$Internal$Button$Light));
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Checkbox = function (a) {
	return {$: 'Checkbox', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$create = F2(
	function (options, label) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Checkbox(
			{label: label, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Id':
				var val = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(val)
					});
			case 'Value':
				var val = modifier.a;
				return _Utils_update(
					options,
					{state: val});
			case 'Inline':
				return _Utils_update(
					options,
					{inline: true});
			case 'OnChecked':
				var toMsg = modifier.a;
				return _Utils_update(
					options,
					{
						onChecked: $elm$core$Maybe$Just(toMsg)
					});
			case 'Custom':
				return _Utils_update(
					options,
					{custom: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			case 'Validation':
				var validation = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation)
					});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Off = {$: 'Off'};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$defaultOptions = {attributes: _List_Nil, custom: false, disabled: false, id: $elm$core$Maybe$Nothing, inline: false, onChecked: $elm$core$Maybe$Nothing, state: $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Off, validation: $elm$core$Maybe$Nothing};
var $elm$html$Html$Attributes$for = $elm$html$Html$Attributes$stringProperty('htmlFor');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$core$Basics$not = _Basics_not;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$targetChecked = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'checked']),
	$elm$json$Json$Decode$bool);
var $elm$html$Html$Events$onCheck = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetChecked));
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$stateAttribute = function (state) {
	switch (state.$) {
		case 'On':
			return $elm$html$Html$Attributes$checked(true);
		case 'Off':
			return $elm$html$Html$Attributes$checked(false);
		default:
			return A2($elm$html$Html$Attributes$attribute, 'indeterminate', 'true');
	}
};
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString = function (validation) {
	if (validation.$ === 'Success') {
		return 'is-valid';
	} else {
		return 'is-invalid';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$toAttributes = function (options) {
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('form-check-input', !options.custom),
						_Utils_Tuple2('custom-control-input', options.custom)
					])),
				$elm$html$Html$Attributes$type_('checkbox'),
				$elm$html$Html$Attributes$disabled(options.disabled),
				$rundis$elm_bootstrap$Bootstrap$Form$Checkbox$stateAttribute(options.state)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Events$onCheck, options.onChecked),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id)
					])),
			_Utils_ap(
				function () {
					var _v0 = options.validation;
					if (_v0.$ === 'Just') {
						var v = _v0.a;
						return _List_fromArray(
							[
								$elm$html$Html$Attributes$class(
								$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(v))
							]);
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes)));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$view = function (_v0) {
	var chk = _v0.a;
	var opts = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$defaultOptions, chk.options);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('form-check', !opts.custom),
						_Utils_Tuple2('form-check-inline', (!opts.custom) && opts.inline),
						_Utils_Tuple2('custom-control', opts.custom),
						_Utils_Tuple2('custom-checkbox', opts.custom),
						_Utils_Tuple2('custom-control-inline', opts.inline && opts.custom)
					]))
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$input,
				$rundis$elm_bootstrap$Bootstrap$Form$Checkbox$toAttributes(opts),
				_List_Nil),
				A2(
				$elm$html$Html$label,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('form-check-label', !opts.custom),
									_Utils_Tuple2('custom-control-label', opts.custom)
								]))
						]),
					function () {
						var _v1 = opts.id;
						if (_v1.$ === 'Just') {
							var v = _v1.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$for(v)
								]);
						} else {
							return _List_Nil;
						}
					}()),
				_List_fromArray(
					[
						$elm$html$Html$text(chk.label)
					]))
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$checkbox = F2(
	function (options, label) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$view(
			A2($rundis$elm_bootstrap$Bootstrap$Form$Checkbox$create, options, label));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$On = {$: 'On'};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Value = function (a) {
	return {$: 'Value', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$checked = function (isCheck) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Value(
		isCheck ? $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$On : $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$Off);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Col = function (a) {
	return {$: 'Col', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$col = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Col(
			{children: children, elemFn: $elm$html$Html$div, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs = function (a) {
	return {$: 'ColAttrs', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs = function (attrs_) {
	return $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs(attrs_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$colLabel = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Col(
			{
				children: children,
				elemFn: $elm$html$Html$label,
				options: A2(
					$elm$core$List$cons,
					$rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-form-label')
							])),
					options)
			});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$OnChecked = function (a) {
	return {$: 'OnChecked', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$onCheck = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Checkbox$OnChecked(toMsg);
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col = {$: 'Col'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width = F2(
	function (screenSize, columnCount) {
		return {columnCount: columnCount, screenSize: screenSize};
	});
var $rundis$elm_bootstrap$Bootstrap$General$Internal$XS = {$: 'XS'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign = F2(
	function (align_, options) {
		var _v0 = align_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						alignXs: $elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						alignSm: $elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						alignMd: $elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						alignLg: $elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						alignXl: $elm$core$Maybe$Just(align_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset = F2(
	function (offset_, options) {
		var _v0 = offset_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						offsetXs: $elm$core$Maybe$Just(offset_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						offsetSm: $elm$core$Maybe$Just(offset_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						offsetMd: $elm$core$Maybe$Just(offset_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						offsetLg: $elm$core$Maybe$Just(offset_)
					});
			default:
				return _Utils_update(
					options,
					{
						offsetXl: $elm$core$Maybe$Just(offset_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder = F2(
	function (order_, options) {
		var _v0 = order_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						orderXs: $elm$core$Maybe$Just(order_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						orderSm: $elm$core$Maybe$Just(order_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						orderMd: $elm$core$Maybe$Just(order_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						orderLg: $elm$core$Maybe$Just(order_)
					});
			default:
				return _Utils_update(
					options,
					{
						orderXl: $elm$core$Maybe$Just(order_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull = F2(
	function (pull_, options) {
		var _v0 = pull_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pullXs: $elm$core$Maybe$Just(pull_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pullSm: $elm$core$Maybe$Just(pull_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pullMd: $elm$core$Maybe$Just(pull_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pullLg: $elm$core$Maybe$Just(pull_)
					});
			default:
				return _Utils_update(
					options,
					{
						pullXl: $elm$core$Maybe$Just(pull_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush = F2(
	function (push_, options) {
		var _v0 = push_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pushXs: $elm$core$Maybe$Just(push_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pushSm: $elm$core$Maybe$Just(push_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pushMd: $elm$core$Maybe$Just(push_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pushLg: $elm$core$Maybe$Just(push_)
					});
			default:
				return _Utils_update(
					options,
					{
						pushXl: $elm$core$Maybe$Just(push_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth = F2(
	function (width_, options) {
		var _v0 = width_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						widthXs: $elm$core$Maybe$Just(width_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						widthSm: $elm$core$Maybe$Just(width_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						widthMd: $elm$core$Maybe$Just(width_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						widthLg: $elm$core$Maybe$Just(width_)
					});
			default:
				return _Utils_update(
					options,
					{
						widthXl: $elm$core$Maybe$Just(width_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'ColAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'ColWidth':
				var width_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth, width_, options);
			case 'ColOffset':
				var offset_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset, offset_, options);
			case 'ColPull':
				var pull_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull, pull_, options);
			case 'ColPush':
				var push_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush, push_, options);
			case 'ColOrder':
				var order_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder, order_, options);
			case 'ColAlign':
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign, align, options);
			default:
				var align = modifier.a;
				return _Utils_update(
					options,
					{
						textAlign: $elm$core$Maybe$Just(align)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption = function (size) {
	switch (size.$) {
		case 'Col':
			return $elm$core$Maybe$Nothing;
		case 'Col1':
			return $elm$core$Maybe$Just('1');
		case 'Col2':
			return $elm$core$Maybe$Just('2');
		case 'Col3':
			return $elm$core$Maybe$Just('3');
		case 'Col4':
			return $elm$core$Maybe$Just('4');
		case 'Col5':
			return $elm$core$Maybe$Just('5');
		case 'Col6':
			return $elm$core$Maybe$Just('6');
		case 'Col7':
			return $elm$core$Maybe$Just('7');
		case 'Col8':
			return $elm$core$Maybe$Just('8');
		case 'Col9':
			return $elm$core$Maybe$Just('9');
		case 'Col10':
			return $elm$core$Maybe$Just('10');
		case 'Col11':
			return $elm$core$Maybe$Just('11');
		case 'Col12':
			return $elm$core$Maybe$Just('12');
		default:
			return $elm$core$Maybe$Just('auto');
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass = function (_v0) {
	var screenSize = _v0.screenSize;
	var columnCount = _v0.columnCount;
	return $elm$html$Html$Attributes$class(
		'col' + (A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption(columnCount)))));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes = function (widths) {
	var width_ = function (w) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass, w);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, width_, widths));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions = {alignLg: $elm$core$Maybe$Nothing, alignMd: $elm$core$Maybe$Nothing, alignSm: $elm$core$Maybe$Nothing, alignXl: $elm$core$Maybe$Nothing, alignXs: $elm$core$Maybe$Nothing, attributes: _List_Nil, offsetLg: $elm$core$Maybe$Nothing, offsetMd: $elm$core$Maybe$Nothing, offsetSm: $elm$core$Maybe$Nothing, offsetXl: $elm$core$Maybe$Nothing, offsetXs: $elm$core$Maybe$Nothing, orderLg: $elm$core$Maybe$Nothing, orderMd: $elm$core$Maybe$Nothing, orderSm: $elm$core$Maybe$Nothing, orderXl: $elm$core$Maybe$Nothing, orderXs: $elm$core$Maybe$Nothing, pullLg: $elm$core$Maybe$Nothing, pullMd: $elm$core$Maybe$Nothing, pullSm: $elm$core$Maybe$Nothing, pullXl: $elm$core$Maybe$Nothing, pullXs: $elm$core$Maybe$Nothing, pushLg: $elm$core$Maybe$Nothing, pushMd: $elm$core$Maybe$Nothing, pushSm: $elm$core$Maybe$Nothing, pushXl: $elm$core$Maybe$Nothing, pushXs: $elm$core$Maybe$Nothing, textAlign: $elm$core$Maybe$Nothing, widthLg: $elm$core$Maybe$Nothing, widthMd: $elm$core$Maybe$Nothing, widthSm: $elm$core$Maybe$Nothing, widthXl: $elm$core$Maybe$Nothing, widthXs: $elm$core$Maybe$Nothing};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption = function (size) {
	switch (size.$) {
		case 'Offset0':
			return '0';
		case 'Offset1':
			return '1';
		case 'Offset2':
			return '2';
		case 'Offset3':
			return '3';
		case 'Offset4':
			return '4';
		case 'Offset5':
			return '5';
		case 'Offset6':
			return '6';
		case 'Offset7':
			return '7';
		case 'Offset8':
			return '8';
		case 'Offset9':
			return '9';
		case 'Offset10':
			return '10';
		default:
			return '11';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString = function (screenSize) {
	var _v0 = $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize);
	if (_v0.$ === 'Just') {
		var s = _v0.a;
		return '-' + (s + '-');
	} else {
		return '-';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass = function (_v0) {
	var screenSize = _v0.screenSize;
	var offsetCount = _v0.offsetCount;
	return $elm$html$Html$Attributes$class(
		'offset' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption(offsetCount)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes = function (offsets) {
	var offset_ = function (m) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass, m);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, offset_, offsets));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption = function (size) {
	switch (size.$) {
		case 'OrderFirst':
			return 'first';
		case 'Order1':
			return '1';
		case 'Order2':
			return '2';
		case 'Order3':
			return '3';
		case 'Order4':
			return '4';
		case 'Order5':
			return '5';
		case 'Order6':
			return '6';
		case 'Order7':
			return '7';
		case 'Order8':
			return '8';
		case 'Order9':
			return '9';
		case 'Order10':
			return '10';
		case 'Order11':
			return '11';
		case 'Order12':
			return '12';
		default:
			return 'last';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes = function (orders) {
	var order_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'order' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, order_, orders));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption = function (size) {
	switch (size.$) {
		case 'Move0':
			return '0';
		case 'Move1':
			return '1';
		case 'Move2':
			return '2';
		case 'Move3':
			return '3';
		case 'Move4':
			return '4';
		case 'Move5':
			return '5';
		case 'Move6':
			return '6';
		case 'Move7':
			return '7';
		case 'Move8':
			return '8';
		case 'Move9':
			return '9';
		case 'Move10':
			return '10';
		case 'Move11':
			return '11';
		default:
			return '12';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes = function (pulls) {
	var pull_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'pull' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, pull_, pulls));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes = function (pushes) {
	var push_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'push' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, push_, pushes));
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir.$) {
		case 'Center':
			return 'center';
		case 'Left':
			return 'left';
		default:
			return 'right';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_v0) {
	var dir = _v0.dir;
	var size = _v0.size;
	return $elm$html$Html$Attributes$class(
		'text' + (A2(
			$elm$core$Maybe$withDefault,
			'-',
			A2(
				$elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption = function (align) {
	switch (align.$) {
		case 'Top':
			return 'start';
		case 'Middle':
			return 'center';
		default:
			return 'end';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass = F2(
	function (prefix, _v0) {
		var align = _v0.align;
		var screenSize = _v0.screenSize;
		return $elm$html$Html$Attributes$class(
			_Utils_ap(
				prefix,
				_Utils_ap(
					A2(
						$elm$core$Maybe$withDefault,
						'',
						A2(
							$elm$core$Maybe$map,
							function (v) {
								return v + '-';
							},
							$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))),
					$rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption(align))));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes = F2(
	function (prefix, aligns) {
		var align = function (a) {
			return A2(
				$elm$core$Maybe$map,
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass(prefix),
				a);
		};
		return A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2($elm$core$List$map, align, aligns));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions, modifiers);
	var shouldAddDefaultXs = !$elm$core$List$length(
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[options.widthXs, options.widthSm, options.widthMd, options.widthLg, options.widthXl])));
	return _Utils_ap(
		$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes(
			_List_fromArray(
				[
					shouldAddDefaultXs ? $elm$core$Maybe$Just(
					A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col)) : options.widthXs,
					options.widthSm,
					options.widthMd,
					options.widthLg,
					options.widthXl
				])),
		_Utils_ap(
			$rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes(
				_List_fromArray(
					[options.offsetXs, options.offsetSm, options.offsetMd, options.offsetLg, options.offsetXl])),
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes(
					_List_fromArray(
						[options.pullXs, options.pullSm, options.pullMd, options.pullLg, options.pullXl])),
				_Utils_ap(
					$rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes(
						_List_fromArray(
							[options.pushXs, options.pushSm, options.pushMd, options.pushLg, options.pushXl])),
					_Utils_ap(
						$rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes(
							_List_fromArray(
								[options.orderXs, options.orderSm, options.orderMd, options.orderLg, options.orderXl])),
						_Utils_ap(
							A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
								'align-self-',
								_List_fromArray(
									[options.alignXs, options.alignSm, options.alignMd, options.alignLg, options.alignXl])),
							_Utils_ap(
								function () {
									var _v0 = options.textAlign;
									if (_v0.$ === 'Just') {
										var a = _v0.a;
										return _List_fromArray(
											[
												$rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(a)
											]);
									} else {
										return _List_Nil;
									}
								}(),
								options.attributes)))))));
};
var $rundis$elm_bootstrap$Bootstrap$Form$renderCol = function (_v0) {
	var elemFn = _v0.a.elemFn;
	var options = _v0.a.options;
	var children = _v0.a.children;
	return A2(
		elemFn,
		$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
		children);
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign = F2(
	function (align, options) {
		var _v0 = align.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						hAlignXs: $elm$core$Maybe$Just(align)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						hAlignSm: $elm$core$Maybe$Just(align)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						hAlignMd: $elm$core$Maybe$Just(align)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						hAlignLg: $elm$core$Maybe$Just(align)
					});
			default:
				return _Utils_update(
					options,
					{
						hAlignXl: $elm$core$Maybe$Just(align)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign = F2(
	function (align_, options) {
		var _v0 = align_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						vAlignXs: $elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						vAlignSm: $elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						vAlignMd: $elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						vAlignLg: $elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						vAlignXl: $elm$core$Maybe$Just(align_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'RowAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'RowVAlign':
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign, align, options);
			default:
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign, align, options);
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions = {attributes: _List_Nil, hAlignLg: $elm$core$Maybe$Nothing, hAlignMd: $elm$core$Maybe$Nothing, hAlignSm: $elm$core$Maybe$Nothing, hAlignXl: $elm$core$Maybe$Nothing, hAlignXs: $elm$core$Maybe$Nothing, vAlignLg: $elm$core$Maybe$Nothing, vAlignMd: $elm$core$Maybe$Nothing, vAlignSm: $elm$core$Maybe$Nothing, vAlignXl: $elm$core$Maybe$Nothing, vAlignXs: $elm$core$Maybe$Nothing};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption = function (align) {
	switch (align.$) {
		case 'Left':
			return 'start';
		case 'Center':
			return 'center';
		case 'Right':
			return 'end';
		case 'Around':
			return 'around';
		default:
			return 'between';
	}
};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass = function (_v0) {
	var align = _v0.align;
	var screenSize = _v0.screenSize;
	return $elm$html$Html$Attributes$class(
		'justify-content-' + (A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return v + '-';
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + $rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption(align)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes = function (aligns) {
	var align = function (a) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass, a);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, align, aligns));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('row')
			]),
		_Utils_ap(
			A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
				'align-items-',
				_List_fromArray(
					[options.vAlignXs, options.vAlignSm, options.vAlignMd, options.vAlignLg, options.vAlignXl])),
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes(
					_List_fromArray(
						[options.hAlignXs, options.hAlignSm, options.hAlignMd, options.hAlignLg, options.hAlignXl])),
				options.attributes)));
};
var $rundis$elm_bootstrap$Bootstrap$Form$row = F2(
	function (options, cols) {
		return A2(
			$elm$html$Html$div,
			A2(
				$elm$core$List$cons,
				$elm$html$Html$Attributes$class('form-group'),
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options)),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Form$renderCol, cols));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col10 = {$: 'Col10'};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$SM = {$: 'SM'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth = function (a) {
	return {$: 'ColWidth', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$width = F2(
	function (size, count) {
		return $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth(
			A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, size, count));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col10);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2 = {$: 'Col2'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2);
var $author$project$Main$mkFormRowCheckbox = F3(
	function (label, state, msg) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$Checkbox$checkbox,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Checkbox$checked(state),
									$rundis$elm_bootstrap$Bootstrap$Form$Checkbox$onCheck(msg)
								]),
							'')
						]))
				]));
	});
var $author$project$Main$mkFormRowHtml = F2(
	function (label, html) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[html]))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Item = function (a) {
	return {$: 'Item', a: a};
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $rundis$elm_bootstrap$Bootstrap$Form$Select$item = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Select$Item(
			A2($elm$html$Html$option, attributes, children));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Select$OnChange = function (a) {
	return {$: 'OnChange', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$onChange = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Select$OnChange(toMsg);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Select = function (a) {
	return {$: 'Select', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$create = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Select$Select(
			{items: items, options: options});
	});
var $elm$html$Html$select = _VirtualDom_node('select');
var $rundis$elm_bootstrap$Bootstrap$Form$Select$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size_ = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size_)
					});
			case 'Id':
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(id_)
					});
			case 'Custom':
				return _Utils_update(
					options,
					{custom: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			case 'OnChange':
				var onChange_ = modifier.a;
				return _Utils_update(
					options,
					{
						onChange: $elm$core$Maybe$Just(onChange_)
					});
			case 'Validation':
				var validation_ = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation_)
					});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rundis$elm_bootstrap$Bootstrap$Form$Select$customEventOnChange = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$defaultOptions = {attributes: _List_Nil, custom: false, disabled: false, id: $elm$core$Maybe$Nothing, onChange: $elm$core$Maybe$Nothing, size: $elm$core$Maybe$Nothing, validation: $elm$core$Maybe$Nothing};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$sizeAttribute = F2(
	function (isCustom, size_) {
		var prefix = isCustom ? 'custom-select-' : 'form-control-';
		return A2(
			$elm$core$Maybe$map,
			function (s) {
				return $elm$html$Html$Attributes$class(
					_Utils_ap(prefix, s));
			},
			$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size_));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Select$validationAttribute = function (validation_) {
	return $elm$html$Html$Attributes$class(
		$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation_));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Select$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Select$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('form-control', !options.custom),
						_Utils_Tuple2('custom-select', options.custom)
					])),
				$elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id),
						A2(
						$elm$core$Maybe$andThen,
						$rundis$elm_bootstrap$Bootstrap$Form$Select$sizeAttribute(options.custom),
						options.size),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Select$customEventOnChange, options.onChange),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Select$validationAttribute, options.validation)
					])),
			options.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$view = function (_v0) {
	var options = _v0.a.options;
	var items = _v0.a.items;
	return A2(
		$elm$html$Html$select,
		$rundis$elm_bootstrap$Bootstrap$Form$Select$toAttributes(options),
		A2(
			$elm$core$List$map,
			function (_v1) {
				var e = _v1.a;
				return e;
			},
			items));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$select = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Select$view(
			A2($rundis$elm_bootstrap$Bootstrap$Form$Select$create, options, items));
	});
var $elm$html$Html$Attributes$selected = $elm$html$Html$Attributes$boolProperty('selected');
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $author$project$Main$mkFormRowSelect = F4(
	function (label, optList, curOpt, msg) {
		var mkOption = function (val) {
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$value(val),
						$elm$html$Html$Attributes$selected(
						_Utils_eq(val, curOpt))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(val)
					]));
		};
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$Select$select,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Select$onChange(msg)
								]),
							A2($elm$core$List$map, mkOption, optList))
						]))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$attrs = function (attrs_) {
	return $rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs(attrs_);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $rundis$elm_bootstrap$Bootstrap$Button$onClick = function (message) {
	return $rundis$elm_bootstrap$Bootstrap$Button$attrs(
		_List_fromArray(
			[
				A2(
				$elm$html$Html$Events$preventDefaultOn,
				'click',
				$elm$json$Json$Decode$succeed(
					_Utils_Tuple2(message, true)))
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$OnInput = function (a) {
	return {$: 'OnInput', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$onInput = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Input$OnInput(toMsg);
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8 = {$: 'Col8'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8);
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Text = {$: 'Text'};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Input = function (a) {
	return {$: 'Input', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Type = function (a) {
	return {$: 'Type', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$create = F2(
	function (tipe, options) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Input$Input(
			{
				options: A2(
					$elm$core$List$cons,
					$rundis$elm_bootstrap$Bootstrap$Form$Input$Type(tipe),
					options)
			});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size_ = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size_)
					});
			case 'Id':
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(id_)
					});
			case 'Type':
				var tipe = modifier.a;
				return _Utils_update(
					options,
					{tipe: tipe});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			case 'Value':
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						value: $elm$core$Maybe$Just(value_)
					});
			case 'Placeholder':
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						placeholder: $elm$core$Maybe$Just(value_)
					});
			case 'OnInput':
				var onInput_ = modifier.a;
				return _Utils_update(
					options,
					{
						onInput: $elm$core$Maybe$Just(onInput_)
					});
			case 'Validation':
				var validation_ = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation_)
					});
			case 'Readonly':
				var val = modifier.a;
				return _Utils_update(
					options,
					{readonly: val});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions = {attributes: _List_Nil, disabled: false, id: $elm$core$Maybe$Nothing, onInput: $elm$core$Maybe$Nothing, placeholder: $elm$core$Maybe$Nothing, readonly: false, size: $elm$core$Maybe$Nothing, tipe: $rundis$elm_bootstrap$Bootstrap$Form$Input$Text, validation: $elm$core$Maybe$Nothing, value: $elm$core$Maybe$Nothing};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$readonly = $elm$html$Html$Attributes$boolProperty('readOnly');
var $rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute = function (size) {
	return A2(
		$elm$core$Maybe$map,
		function (s) {
			return $elm$html$Html$Attributes$class('form-control-' + s);
		},
		$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute = function (inputType) {
	return $elm$html$Html$Attributes$type_(
		function () {
			switch (inputType.$) {
				case 'Text':
					return 'text';
				case 'Password':
					return 'password';
				case 'DatetimeLocal':
					return 'datetime-local';
				case 'Date':
					return 'date';
				case 'Month':
					return 'month';
				case 'Time':
					return 'time';
				case 'Week':
					return 'week';
				case 'Number':
					return 'number';
				case 'Email':
					return 'email';
				case 'Url':
					return 'url';
				case 'Search':
					return 'search';
				case 'Tel':
					return 'tel';
				default:
					return 'color';
			}
		}());
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute = function (validation) {
	return $elm$html$Html$Attributes$class(
		$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('form-control'),
				$elm$html$Html$Attributes$disabled(options.disabled),
				$elm$html$Html$Attributes$readonly(options.readonly),
				$rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute(options.tipe)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id),
						A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute, options.size),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$value, options.value),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$placeholder, options.placeholder),
						A2($elm$core$Maybe$map, $elm$html$Html$Events$onInput, options.onInput),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute, options.validation)
					])),
			options.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$view = function (_v0) {
	var options = _v0.a.options;
	return A2(
		$elm$html$Html$input,
		$rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes(options),
		_List_Nil);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$input = F2(
	function (tipe, options) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Input$view(
			A2($rundis$elm_bootstrap$Bootstrap$Form$Input$create, tipe, options));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$text = $rundis$elm_bootstrap$Bootstrap$Form$Input$input($rundis$elm_bootstrap$Bootstrap$Form$Input$Text);
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Value = function (a) {
	return {$: 'Value', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$value = function (value_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Input$Value(value_);
};
var $author$project$Main$mkFormRowTextEntrySubmit = F4(
	function (label, defValue, inputMsg, submitMsg) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8]),
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Form$Input$text(
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Input$value(defValue),
									$rundis$elm_bootstrap$Bootstrap$Form$Input$onInput(inputMsg)
								]))
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Button$button,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Button$onClick(submitMsg)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Add')
								]))
						]))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Size = function (a) {
	return {$: 'Size', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$small = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Size($rundis$elm_bootstrap$Bootstrap$General$Internal$SM);
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$validAccessSpecOptions = _List_fromArray(
	['', 'VIEW', 'UPLOAD', 'CONTRIBUTE', 'ADMINISTER']);
var $author$project$Main$paneAccessSpec = function (model) {
	var mkLi = function (val) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(val),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Button$button,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Button$small,
							$rundis$elm_bootstrap$Bootstrap$Button$light,
							$rundis$elm_bootstrap$Bootstrap$Button$onClick(
							$author$project$Main$DeleteAccessSpecNetwork(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Delete')
						]))
				]));
	};
	var curNetworks = function (spec) {
		var _v1 = $elm$core$List$isEmpty(spec.network);
		if (_v1) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2($elm$core$List$map, mkLi, spec.network));
		}
	};
	var app = model.app;
	var form = function () {
		var _v0 = app.access;
		if (_v0.$ === 'Just') {
			var spec = _v0.a;
			return _List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$form,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$author$project$Main$mkFormRowHtml,
							'Allowed Networks',
							curNetworks(spec)),
							A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Network', model.accessSpecNetwork, $author$project$Main$UpdateAccessSpecNetwork, $author$project$Main$UpdateAccessSpecAddNetwork),
							A4(
							$author$project$Main$mkFormRowSelect,
							'Project',
							$author$project$Main$validAccessSpecOptions,
							A2($elm$core$Maybe$withDefault, '', spec.project),
							$author$project$Main$UpdateAccessSpecProject),
							A4(
							$author$project$Main$mkFormRowSelect,
							'All Projects',
							$author$project$Main$validAccessSpecOptions,
							A2($elm$core$Maybe$withDefault, '', spec.allProjects),
							$author$project$Main$UpdateAccessSpecAllProjects),
							A3(
							$author$project$Main$mkFormRowCheckbox,
							'Developer',
							A2($elm$core$Maybe$withDefault, false, spec.developer),
							$author$project$Main$UpdateAccessSpecDeveloper),
							A3(
							$author$project$Main$mkFormRowCheckbox,
							'Project Creation',
							A2($elm$core$Maybe$withDefault, false, spec.projectCreation),
							$author$project$Main$UpdateAccessSpecProjectCreation)
						]))
				]);
		} else {
			return _List_Nil;
		}
	}();
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_Utils_ap(
			_List_fromArray(
				[
					A3($author$project$Main$mkFormRowCheckbox, 'Enable', model.enableAccess, $author$project$Main$EnableAccess)
				]),
			form));
};
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Main$paneDebug = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(
				$elm$core$Debug$toString(model))
			]));
};
var $author$project$Main$ModifyExecDependsDialog = F2(
	function (a, b) {
		return {$: 'ModifyExecDependsDialog', a: a, b: b};
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Column = function (a) {
	return {$: 'Column', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$col = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Grid$Column(
			{children: children, options: options});
	});
var $author$project$Main$DeleteExecDepends = function (a) {
	return {$: 'DeleteExecDepends', a: a};
};
var $author$project$Main$SetExecDependsToModify = function (a) {
	return {$: 'SetExecDependsToModify', a: a};
};
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $author$project$Main$execDependsTable = function (execDepends) {
	var inputTr = F2(
		function (index, dep) {
			return A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(dep.name)
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$Main$packageManagerToString(dep.packageManager))
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(dep.version)
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A2($elm$core$String$join, ', ', dep.stages))
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('btn btn-default'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$SetExecDependsToModify(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('btn btn-default'),
										$elm$html$Html$Events$onClick(
										$author$project$Main$DeleteExecDepends(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var tbl = A2(
		$elm$html$Html$table,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('table')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$thead,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$tr,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Name')
									])),
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Package Manager')
									])),
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Version')
									])),
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Stages')
									])),
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('')
									])),
								A2(
								$elm$html$Html$th,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('')
									]))
							]))
					])),
				A2(
				$elm$html$Html$tbody,
				_List_Nil,
				A2($elm$core$List$indexedMap, inputTr, execDepends))
			]));
	var _v0 = $elm$core$List$isEmpty(execDepends);
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No dependencies')
				]));
	} else {
		return tbl;
	}
};
var $author$project$Main$initialExecDepends = {name: 'package_name', packageManager: $author$project$Main$Apt, stages: _List_Nil, version: ''};
var $author$project$Main$CloseExecDependsDialog = {$: 'CloseExecDependsDialog'};
var $author$project$Main$DeleteExecDependsStage = function (a) {
	return {$: 'DeleteExecDependsStage', a: a};
};
var $author$project$Main$SaveExecDepends = {$: 'SaveExecDepends'};
var $author$project$Main$UpdateExecDependsAddStage = {$: 'UpdateExecDependsAddStage'};
var $author$project$Main$UpdateExecDependsName = function (a) {
	return {$: 'UpdateExecDependsName', a: a};
};
var $author$project$Main$UpdateExecDependsPackageManager = function (a) {
	return {$: 'UpdateExecDependsPackageManager', a: a};
};
var $author$project$Main$UpdateExecDependsStage = function (a) {
	return {$: 'UpdateExecDependsStage', a: a};
};
var $author$project$Main$UpdateExecDependsVersion = function (a) {
	return {$: 'UpdateExecDependsVersion', a: a};
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$mkTh = function (label) {
	return A2(
		$elm$html$Html$th,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'align', 'right')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(label)
			]));
};
var $author$project$Main$mkRowHtml = F2(
	function (label, html) {
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$mkTh(label),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[html]))
				]));
	});
var $author$project$Main$mkRowSelect = F4(
	function (label, optList, curOpt, msg) {
		var mkOption = function (val) {
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$value(val),
						$elm$html$Html$Attributes$selected(
						_Utils_eq(val, curOpt))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(val)
					]));
		};
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$mkTh(label),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$Select$select,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Select$onChange(msg)
								]),
							A2($elm$core$List$map, mkOption, optList))
						]))
				]));
	});
var $elm$html$Html$Attributes$size = function (n) {
	return A2(
		_VirtualDom_attribute,
		'size',
		$elm$core$String$fromInt(n));
};
var $author$project$Main$mkRowTextEntry = F3(
	function (label, defValue, msg) {
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$mkTh(label),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('text'),
									$elm$html$Html$Attributes$value(defValue),
									$elm$html$Html$Attributes$class('form-control'),
									$elm$html$Html$Events$onInput(msg),
									$elm$html$Html$Attributes$size(60)
								]),
							_List_Nil)
						]))
				]));
	});
var $author$project$Main$mkRowTextEntrySubmit = F4(
	function (label, defValue, inputMsg, submitMsg) {
		return A2(
			$elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					$author$project$Main$mkTh(label),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$input,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$type_('text'),
									$elm$html$Html$Attributes$value(defValue),
									$elm$html$Html$Attributes$class('form-control'),
									$elm$html$Html$Events$onInput(inputMsg),
									$elm$html$Html$Attributes$size(60)
								]),
							_List_Nil)
						])),
					A2(
					$elm$html$Html$td,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$button,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('btn btn-default'),
									$elm$html$Html$Attributes$type_('button'),
									$elm$html$Html$Events$onClick(submitMsg)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Add')
								]))
						]))
				]));
	});
var $alex_tan$elm_dialog$Dialog$isJust = function (m) {
	if (m.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $alex_tan$elm_dialog$Dialog$backdrop = function (config) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'modal-backdrop in',
						$alex_tan$elm_dialog$Dialog$isJust(config))
					]))
			]),
		_List_Nil);
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $alex_tan$elm_dialog$Dialog$empty = A2($elm$html$Html$span, _List_Nil, _List_Nil);
var $alex_tan$elm_dialog$Dialog$maybe = F3(
	function (_default, f, value) {
		if (value.$ === 'Just') {
			var value_ = value.a;
			return f(value_);
		} else {
			return _default;
		}
	});
var $alex_tan$elm_dialog$Dialog$wrapBody = function (body) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('modal-body')
			]),
		_List_fromArray(
			[body]));
};
var $alex_tan$elm_dialog$Dialog$wrapFooter = function (footer) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('modal-footer')
			]),
		_List_fromArray(
			[footer]));
};
var $alex_tan$elm_dialog$Dialog$closeButton = function (closeMessage) {
	return A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('close'),
				$elm$html$Html$Events$onClick(closeMessage)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('x')
			]));
};
var $alex_tan$elm_dialog$Dialog$wrapHeader = F2(
	function (closeMessage, header) {
		return (_Utils_eq(closeMessage, $elm$core$Maybe$Nothing) && _Utils_eq(header, $elm$core$Maybe$Nothing)) ? $alex_tan$elm_dialog$Dialog$empty : A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('modal-header')
				]),
			_List_fromArray(
				[
					A3($alex_tan$elm_dialog$Dialog$maybe, $alex_tan$elm_dialog$Dialog$empty, $alex_tan$elm_dialog$Dialog$closeButton, closeMessage),
					A2($elm$core$Maybe$withDefault, $alex_tan$elm_dialog$Dialog$empty, header)
				]));
	});
var $alex_tan$elm_dialog$Dialog$view = function (maybeConfig) {
	var displayed = $alex_tan$elm_dialog$Dialog$isJust(maybeConfig);
	return A2(
		$elm$html$Html$div,
		function () {
			var _v0 = A2(
				$elm$core$Maybe$andThen,
				function ($) {
					return $.containerClass;
				},
				maybeConfig);
			if (_v0.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var containerClass = _v0.a;
				return _List_fromArray(
					[
						$elm$html$Html$Attributes$class(containerClass)
					]);
			}
		}(),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2('in', displayed)
							])),
						A2(
						$elm$html$Html$Attributes$style,
						'display',
						displayed ? 'block' : 'none')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('modal-dialog')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('modal-content')
									]),
								function () {
									if (maybeConfig.$ === 'Nothing') {
										return _List_fromArray(
											[$alex_tan$elm_dialog$Dialog$empty]);
									} else {
										var config = maybeConfig.a;
										return _List_fromArray(
											[
												A2($alex_tan$elm_dialog$Dialog$wrapHeader, config.closeMessage, config.header),
												A3($alex_tan$elm_dialog$Dialog$maybe, $alex_tan$elm_dialog$Dialog$empty, $alex_tan$elm_dialog$Dialog$wrapBody, config.body),
												A3($alex_tan$elm_dialog$Dialog$maybe, $alex_tan$elm_dialog$Dialog$empty, $alex_tan$elm_dialog$Dialog$wrapFooter, config.footer)
											]);
									}
								}())
							]))
					])),
				$alex_tan$elm_dialog$Dialog$backdrop(maybeConfig)
			]));
};
var $author$project$Main$modifyExecDependsDialog = function (model) {
	var packageManagers = A2(
		$elm$core$List$map,
		$author$project$Main$packageManagerToString,
		_List_fromArray(
			[$author$project$Main$Apt, $author$project$Main$Cpan, $author$project$Main$Cran, $author$project$Main$Gem, $author$project$Main$Pip]));
	var mkLi = function (val) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(val),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('btn btn-default'),
							$elm$html$Html$Events$onClick(
							$author$project$Main$DeleteExecDependsStage(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Delete')
						]))
				]));
	};
	var curStages = function (stages) {
		var _v0 = $elm$core$List$length(stages) > 0;
		if (_v0) {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2($elm$core$List$map, mkLi, stages));
		} else {
			return $elm$html$Html$text('None');
		}
	};
	var tbl = function (execDepends) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
							A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$form,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$table,
									_List_Nil,
									_List_fromArray(
										[
											A3($author$project$Main$mkRowTextEntry, 'Name', execDepends.name, $author$project$Main$UpdateExecDependsName),
											A4(
											$author$project$Main$mkRowSelect,
											'Package Manager',
											packageManagers,
											$author$project$Main$packageManagerToString(execDepends.packageManager),
											$author$project$Main$UpdateExecDependsPackageManager),
											A3($author$project$Main$mkRowTextEntry, 'Version', execDepends.version, $author$project$Main$UpdateExecDependsVersion),
											A2(
											$author$project$Main$mkRowHtml,
											'Stages',
											curStages(execDepends.stages)),
											A4($author$project$Main$mkRowTextEntrySubmit, 'Add Stage', model.execDependsStage, $author$project$Main$UpdateExecDependsStage, $author$project$Main$UpdateExecDependsAddStage)
										]))
								]))
						]))
				]));
	};
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (input) {
				return {
					body: $elm$core$Maybe$Just(
						tbl(input)),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$SaveExecDepends)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseExecDependsDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Add Input'))
				};
			},
			model.execDependsToModify));
};
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $rundis$elm_bootstrap$Bootstrap$Grid$renderCol = function (column) {
	switch (column.$) {
		case 'Column':
			var options = column.a.options;
			var children = column.a.children;
			return A2(
				$elm$html$Html$div,
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
		case 'ColBreak':
			var e = column.a;
			return e;
		default:
			var options = column.a.options;
			var children = column.a.children;
			return A3(
				$elm$html$Html$Keyed$node,
				'div',
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$row = F2(
	function (options, cols) {
		return A2(
			$elm$html$Html$div,
			$rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Grid$renderCol, cols));
	});
var $author$project$Main$paneExecDepends = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										A2($author$project$Main$ModifyExecDependsDialog, $author$project$Main$initialExecDepends, $elm$core$Maybe$Nothing))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Add Dependency')
									]))
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$execDependsTable(model.app.runSpec.execDepends)
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$modifyExecDependsDialog(model)
							]))
					]))
			]));
};
var $author$project$Main$DeleteHttpsAppPort = function (a) {
	return {$: 'DeleteHttpsAppPort', a: a};
};
var $author$project$Main$EnableHttpsApp = function (a) {
	return {$: 'EnableHttpsApp', a: a};
};
var $author$project$Main$UpdateHttpsAppAddPort = function (a) {
	return {$: 'UpdateHttpsAppAddPort', a: a};
};
var $author$project$Main$UpdateHttpsAppSharedAccess = function (a) {
	return {$: 'UpdateHttpsAppSharedAccess', a: a};
};
var $author$project$Main$paneHttpsApp = function (model) {
	var mkLi = F2(
		function (index, val) {
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(val),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Button$button,
						_List_fromArray(
							[
								$rundis$elm_bootstrap$Bootstrap$Button$small,
								$rundis$elm_bootstrap$Bootstrap$Button$light,
								$rundis$elm_bootstrap$Bootstrap$Button$onClick(
								$author$project$Main$DeleteHttpsAppPort(index))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Delete')
							]))
					]));
		});
	var curPorts = function (ports) {
		var _v1 = $elm$core$List$isEmpty(ports);
		if (_v1) {
			return $elm$html$Html$text('No ports');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					mkLi,
					A2($elm$core$List$map, $elm$core$String$fromInt, ports)));
		}
	};
	var app = model.app;
	var form = function () {
		var _v0 = app.httpsApp;
		if (_v0.$ === 'Just') {
			var httpsApp = _v0.a;
			var ports = httpsApp.ports;
			return _List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$form,
					_List_Nil,
					_List_fromArray(
						[
							A4(
							$author$project$Main$mkFormRowSelect,
							'Add Port',
							A2($elm$core$List$map, $elm$core$String$fromInt, $author$project$Main$validHttpsPorts),
							'',
							$author$project$Main$UpdateHttpsAppAddPort),
							A2(
							$author$project$Main$mkFormRowHtml,
							'Ports',
							curPorts(httpsApp.ports)),
							A4(
							$author$project$Main$mkFormRowSelect,
							'Shared Access',
							_List_fromArray(
								['VIEW', 'CONTRIBUTE', 'ADMINISTER', 'NONE']),
							httpsApp.sharedAccess,
							$author$project$Main$UpdateHttpsAppSharedAccess)
						]))
				]);
		} else {
			return _List_Nil;
		}
	}();
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_Utils_ap(
			_List_fromArray(
				[
					A3($author$project$Main$mkFormRowCheckbox, 'Enable', model.enableHttpsApp, $author$project$Main$EnableHttpsApp)
				]),
			form));
};
var $author$project$Main$ModifyInputSpecDialog = F2(
	function (a, b) {
		return {$: 'ModifyInputSpecDialog', a: a, b: b};
	});
var $author$project$Main$initialInputSpec = {
	choices: _List_Nil,
	_class: $author$project$Main$InputSpecString,
	_default: $author$project$Main$DefaultValString(''),
	group: '',
	help: '',
	label: 'Human-readable label',
	name: 'input_name',
	optional: false,
	patterns: _List_Nil,
	suggestions: _List_Nil,
	type_: $elm$core$Maybe$Nothing
};
var $author$project$Main$DeleteInputSpec = function (a) {
	return {$: 'DeleteInputSpec', a: a};
};
var $author$project$Main$SetInputSpecToModify = function (a) {
	return {$: 'SetInputSpecToModify', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Danger = {$: 'Danger'};
var $rundis$elm_bootstrap$Bootstrap$Button$danger = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled($rundis$elm_bootstrap$Bootstrap$Internal$Button$Danger));
var $rundis$elm_bootstrap$Bootstrap$Table$Hover = {$: 'Hover'};
var $rundis$elm_bootstrap$Bootstrap$Table$hover = $rundis$elm_bootstrap$Bootstrap$Table$Hover;
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary = {$: 'Primary'};
var $rundis$elm_bootstrap$Bootstrap$Button$primary = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled($rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary));
var $rundis$elm_bootstrap$Bootstrap$Table$THead = function (a) {
	return {$: 'THead', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$thead = F2(
	function (options, rows) {
		return $rundis$elm_bootstrap$Bootstrap$Table$THead(
			{options: options, rows: rows});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Row = function (a) {
	return {$: 'Row', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$tr = F2(
	function (options, cells) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Row(
			{cells: cells, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$simpleThead = function (cells) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Table$thead,
		_List_Nil,
		_List_fromArray(
			[
				A2($rundis$elm_bootstrap$Bootstrap$Table$tr, _List_Nil, cells)
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Table$Striped = {$: 'Striped'};
var $rundis$elm_bootstrap$Bootstrap$Table$striped = $rundis$elm_bootstrap$Bootstrap$Table$Striped;
var $rundis$elm_bootstrap$Bootstrap$Table$Inversed = {$: 'Inversed'};
var $rundis$elm_bootstrap$Bootstrap$Table$isResponsive = function (option) {
	if (option.$ === 'Responsive') {
		return true;
	} else {
		return false;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody = function (a) {
	return {$: 'KeyedTBody', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$TBody = function (a) {
	return {$: 'TBody', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$InversedRow = function (a) {
	return {$: 'InversedRow', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow = function (a) {
	return {$: 'KeyedRow', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$InversedCell = function (a) {
	return {$: 'InversedCell', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$Td = function (a) {
	return {$: 'Td', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$Th = function (a) {
	return {$: 'Th', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell = function (cell) {
	var inverseOptions = function (options) {
		return A2(
			$elm$core$List$map,
			function (opt) {
				if (opt.$ === 'RoledCell') {
					var role = opt.a;
					return $rundis$elm_bootstrap$Bootstrap$Table$InversedCell(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (cell.$ === 'Th') {
		var cellCfg = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellCfg,
				{
					options: inverseOptions(cellCfg.options)
				}));
	} else {
		var cellCfg = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Td(
			_Utils_update(
				cellCfg,
				{
					options: inverseOptions(cellCfg.options)
				}));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow = function (row) {
	var inversedOptions = function (options) {
		return A2(
			$elm$core$List$map,
			function (opt) {
				if (opt.$ === 'RoledRow') {
					var role = opt.a;
					return $rundis$elm_bootstrap$Bootstrap$Table$InversedRow(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		return $rundis$elm_bootstrap$Bootstrap$Table$Row(
			{
				cells: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell, cells),
				options: inversedOptions(options)
			});
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		return $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
			{
				cells: A2(
					$elm$core$List$map,
					function (_v1) {
						var key = _v1.a;
						var cell = _v1.b;
						return _Utils_Tuple2(
							key,
							$rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell(cell));
					},
					cells),
				options: inversedOptions(options)
			});
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody = F2(
	function (isTableInversed, tbody_) {
		var _v0 = _Utils_Tuple2(isTableInversed, tbody_);
		if (!_v0.a) {
			return tbody_;
		} else {
			if (_v0.b.$ === 'TBody') {
				var body = _v0.b.a;
				return $rundis$elm_bootstrap$Bootstrap$Table$TBody(
					_Utils_update(
						body,
						{
							rows: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, body.rows)
						}));
			} else {
				var keyedBody = _v0.b.a;
				return $rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody(
					_Utils_update(
						keyedBody,
						{
							rows: A2(
								$elm$core$List$map,
								function (_v1) {
									var key = _v1.a;
									var row = _v1.b;
									return _Utils_Tuple2(
										key,
										$rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow(row));
								},
								keyedBody.rows)
						}));
			}
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Table$InversedHead = {$: 'InversedHead'};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead = F2(
	function (isTableInversed, _v0) {
		var thead_ = _v0.a;
		var isHeadInversed = A2(
			$elm$core$List$any,
			function (opt) {
				return _Utils_eq(opt, $rundis$elm_bootstrap$Bootstrap$Table$InversedHead);
			},
			thead_.options);
		return $rundis$elm_bootstrap$Bootstrap$Table$THead(
			(isTableInversed || isHeadInversed) ? _Utils_update(
				thead_,
				{
					rows: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, thead_.rows)
				}) : thead_);
	});
var $rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive = F2(
	function (options, table_) {
		var responsiveClass = $elm$html$Html$Attributes$class(
			'table-responsive' + A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function (v) {
						return '-' + v;
					},
					A2(
						$elm$core$Maybe$andThen,
						$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption,
						A2(
							$elm$core$Maybe$andThen,
							function (opt) {
								if (opt.$ === 'Responsive') {
									var val = opt.a;
									return val;
								} else {
									return $elm$core$Maybe$Nothing;
								}
							},
							$elm$core$List$head(
								A2($elm$core$List$filter, $rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options)))))));
		return A2($elm$core$List$any, $rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options) ? A2(
			$elm$html$Html$div,
			_List_fromArray(
				[responsiveClass]),
			_List_fromArray(
				[table_])) : table_;
	});
var $rundis$elm_bootstrap$Bootstrap$Table$CellAttr = function (a) {
	return {$: 'CellAttr', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttr = function (attr_) {
	return $rundis$elm_bootstrap$Bootstrap$Table$CellAttr(attr_);
};
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh = function (cell) {
	if (cell.$ === 'Th') {
		var cellConfig = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellConfig,
				{
					options: A2(
						$elm$core$List$cons,
						$rundis$elm_bootstrap$Bootstrap$Table$cellAttr(
							$elm$html$Html$Attributes$scope('row')),
						cellConfig.options)
				}));
	} else {
		return cell;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell = function (row) {
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		if (!cells.b) {
			return row;
		} else {
			var first = cells.a;
			var rest = cells.b;
			return $rundis$elm_bootstrap$Bootstrap$Table$Row(
				{
					cells: A2(
						$elm$core$List$cons,
						$rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first),
						rest),
					options: options
				});
		}
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		if (!cells.b) {
			return row;
		} else {
			var _v3 = cells.a;
			var firstKey = _v3.a;
			var first = _v3.b;
			var rest = cells.b;
			return $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
				{
					cells: A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							firstKey,
							$rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first)),
						rest),
					options: options
				});
		}
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return $elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role.$) {
					case 'Primary':
						return 'primary';
					case 'Secondary':
						return 'secondary';
					case 'Success':
						return 'success';
					case 'Info':
						return 'info';
					case 'Warning':
						return 'warning';
					case 'Danger':
						return 'danger';
					case 'Light':
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttribute = function (option) {
	switch (option.$) {
		case 'RoledCell':
			if (option.a.$ === 'Roled') {
				var role = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role);
			} else {
				var _v1 = option.a;
				return $elm$html$Html$Attributes$class('table-active');
			}
		case 'InversedCell':
			if (option.a.$ === 'Roled') {
				var role = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg-', role);
			} else {
				var _v2 = option.a;
				return $elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$cellAttribute, options);
};
var $rundis$elm_bootstrap$Bootstrap$Table$renderCell = function (cell) {
	if (cell.$ === 'Td') {
		var options = cell.a.options;
		var children = cell.a.children;
		return A2(
			$elm$html$Html$td,
			$rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	} else {
		var options = cell.a.options;
		var children = cell.a.children;
		return A2(
			$elm$html$Html$th,
			$rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$rowClass = function (option) {
	switch (option.$) {
		case 'RoledRow':
			if (option.a.$ === 'Roled') {
				var role_ = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role_);
			} else {
				var _v1 = option.a;
				return $elm$html$Html$Attributes$class('table-active');
			}
		case 'InversedRow':
			if (option.a.$ === 'Roled') {
				var role_ = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role_);
			} else {
				var _v2 = option.a;
				return $elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$rowAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$rowClass, options);
};
var $rundis$elm_bootstrap$Bootstrap$Table$renderRow = function (row) {
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		return A2(
			$elm$html$Html$tr,
			$rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$renderCell, cells));
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		return A3(
			$elm$html$Html$Keyed$node,
			'tr',
			$rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2(
				$elm$core$List$map,
				function (_v1) {
					var key = _v1.a;
					var cell = _v1.b;
					return _Utils_Tuple2(
						key,
						$rundis$elm_bootstrap$Bootstrap$Table$renderCell(cell));
				},
				cells));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$renderTBody = function (body) {
	if (body.$ === 'TBody') {
		var attributes = body.a.attributes;
		var rows = body.a.rows;
		return A2(
			$elm$html$Html$tbody,
			attributes,
			A2(
				$elm$core$List$map,
				function (row) {
					return $rundis$elm_bootstrap$Bootstrap$Table$renderRow(
						$rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row));
				},
				rows));
	} else {
		var attributes = body.a.attributes;
		var rows = body.a.rows;
		return A3(
			$elm$html$Html$Keyed$node,
			'tbody',
			attributes,
			A2(
				$elm$core$List$map,
				function (_v1) {
					var key = _v1.a;
					var row = _v1.b;
					return _Utils_Tuple2(
						key,
						$rundis$elm_bootstrap$Bootstrap$Table$renderRow(
							$rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row)));
				},
				rows));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$theadAttribute = function (option) {
	switch (option.$) {
		case 'InversedHead':
			return $elm$html$Html$Attributes$class('thead-dark');
		case 'DefaultHead':
			return $elm$html$Html$Attributes$class('thead-default');
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$theadAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$theadAttribute, options);
};
var $rundis$elm_bootstrap$Bootstrap$Table$renderTHead = function (_v0) {
	var options = _v0.a.options;
	var rows = _v0.a.rows;
	return A2(
		$elm$html$Html$thead,
		$rundis$elm_bootstrap$Bootstrap$Table$theadAttributes(options),
		A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$renderRow, rows));
};
var $rundis$elm_bootstrap$Bootstrap$Table$tableClass = function (option) {
	switch (option.$) {
		case 'Inversed':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-dark'));
		case 'Striped':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-striped'));
		case 'Bordered':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-bordered'));
		case 'Hover':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-hover'));
		case 'Small':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-sm'));
		case 'Responsive':
			return $elm$core$Maybe$Nothing;
		case 'Reflow':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-reflow'));
		default:
			var attr_ = option.a;
			return $elm$core$Maybe$Just(attr_);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$tableAttributes = function (options) {
	return A2(
		$elm$core$List$cons,
		$elm$html$Html$Attributes$class('table'),
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$tableClass, options)));
};
var $rundis$elm_bootstrap$Bootstrap$Table$table = function (rec) {
	var isInversed = A2(
		$elm$core$List$any,
		function (opt) {
			return _Utils_eq(opt, $rundis$elm_bootstrap$Bootstrap$Table$Inversed);
		},
		rec.options);
	var classOptions = A2(
		$elm$core$List$filter,
		function (opt) {
			return !$rundis$elm_bootstrap$Bootstrap$Table$isResponsive(opt);
		},
		rec.options);
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive,
		rec.options,
		A2(
			$elm$html$Html$table,
			$rundis$elm_bootstrap$Bootstrap$Table$tableAttributes(classOptions),
			_List_fromArray(
				[
					$rundis$elm_bootstrap$Bootstrap$Table$renderTHead(
					A2($rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead, isInversed, rec.thead)),
					$rundis$elm_bootstrap$Bootstrap$Table$renderTBody(
					A2($rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody, isInversed, rec.tbody))
				])));
};
var $rundis$elm_bootstrap$Bootstrap$Table$tbody = F2(
	function (attributes, rows) {
		return $rundis$elm_bootstrap$Bootstrap$Table$TBody(
			{attributes: attributes, rows: rows});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$td = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Td(
			{children: children, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$th = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			{children: children, options: options});
	});
var $author$project$Main$inputSpecTable = function (inputSpecs) {
	var mkRow = F2(
		function (index, spec) {
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(spec.name)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(spec.label)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								$author$project$Main$inputSpecClassToString(spec._class))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								spec.optional ? 'True' : 'False')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$primary,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$SetInputSpecToModify(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$danger,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$DeleteInputSpec(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var tbl = $rundis$elm_bootstrap$Bootstrap$Table$table(
		{
			options: _List_fromArray(
				[$rundis$elm_bootstrap$Bootstrap$Table$striped, $rundis$elm_bootstrap$Bootstrap$Table$hover]),
			tbody: A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tbody,
				_List_Nil,
				A2($elm$core$List$indexedMap, mkRow, inputSpecs)),
			thead: $rundis$elm_bootstrap$Bootstrap$Table$simpleThead(
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Name')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Label')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Class')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Optional')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							]))
					]))
		});
	var _v0 = $elm$core$List$isEmpty(inputSpecs);
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No input specs')
				]));
	} else {
		return tbl;
	}
};
var $author$project$Main$CloseInputSpecDialog = {$: 'CloseInputSpecDialog'};
var $author$project$Main$DeleteInputSpecChoice = function (a) {
	return {$: 'DeleteInputSpecChoice', a: a};
};
var $author$project$Main$DeleteInputSpecPattern = function (a) {
	return {$: 'DeleteInputSpecPattern', a: a};
};
var $author$project$Main$SaveInputSpec = {$: 'SaveInputSpec'};
var $author$project$Main$SaveInputSpecDefault = {$: 'SaveInputSpecDefault'};
var $author$project$Main$UpdateInputSpecAddChoice = {$: 'UpdateInputSpecAddChoice'};
var $author$project$Main$UpdateInputSpecAddCustomPattern = {$: 'UpdateInputSpecAddCustomPattern'};
var $author$project$Main$UpdateInputSpecAddPattern = function (a) {
	return {$: 'UpdateInputSpecAddPattern', a: a};
};
var $author$project$Main$UpdateInputSpecChoice = function (a) {
	return {$: 'UpdateInputSpecChoice', a: a};
};
var $author$project$Main$UpdateInputSpecClass = function (a) {
	return {$: 'UpdateInputSpecClass', a: a};
};
var $author$project$Main$UpdateInputSpecCustomPattern = function (a) {
	return {$: 'UpdateInputSpecCustomPattern', a: a};
};
var $author$project$Main$UpdateInputSpecDefault = function (a) {
	return {$: 'UpdateInputSpecDefault', a: a};
};
var $author$project$Main$UpdateInputSpecHelp = function (a) {
	return {$: 'UpdateInputSpecHelp', a: a};
};
var $author$project$Main$UpdateInputSpecLabel = function (a) {
	return {$: 'UpdateInputSpecLabel', a: a};
};
var $author$project$Main$UpdateInputSpecName = function (a) {
	return {$: 'UpdateInputSpecName', a: a};
};
var $author$project$Main$UpdateInputSpecOptional = function (a) {
	return {$: 'UpdateInputSpecOptional', a: a};
};
var $author$project$Main$commonInputSpecPatterns = _List_fromArray(
	['*', '*.bai', '*.bam', '*.fa', '*.faa', '*.fasta', '*.fastq', '*.fna', '*.fq', '*.sam']);
var $author$project$Main$mkFormRowText = F2(
	function (label, defValue) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							$elm$html$Html$text(defValue)
						]))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$OnInput = function (a) {
	return {$: 'OnInput', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$onInput = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Textarea$OnInput(toMsg);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Rows = function (a) {
	return {$: 'Rows', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$rows = function (rows_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Rows(rows_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Textarea = function (a) {
	return {$: 'Textarea', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$create = function (options) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Textarea(
		{options: options});
};
var $elm$html$Html$textarea = _VirtualDom_node('textarea');
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Id':
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(id_)
					});
			case 'Rows':
				var rows_ = modifier.a;
				return _Utils_update(
					options,
					{
						rows: $elm$core$Maybe$Just(rows_)
					});
			case 'Disabled':
				return _Utils_update(
					options,
					{disabled: true});
			case 'Value':
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						value: $elm$core$Maybe$Just(value_)
					});
			case 'OnInput':
				var onInput_ = modifier.a;
				return _Utils_update(
					options,
					{
						onInput: $elm$core$Maybe$Just(onInput_)
					});
			case 'Validation':
				var validation = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation)
					});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$defaultOptions = {attributes: _List_Nil, disabled: false, id: $elm$core$Maybe$Nothing, onInput: $elm$core$Maybe$Nothing, rows: $elm$core$Maybe$Nothing, validation: $elm$core$Maybe$Nothing, value: $elm$core$Maybe$Nothing};
var $elm$html$Html$Attributes$rows = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rows',
		$elm$core$String$fromInt(n));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$validationAttribute = function (validation) {
	return $elm$html$Html$Attributes$class(
		$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Textarea$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Textarea$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('form-control'),
				$elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$rows, options.rows),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$value, options.value),
						A2($elm$core$Maybe$map, $elm$html$Html$Events$onInput, options.onInput),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Textarea$validationAttribute, options.validation)
					])),
			options.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$view = function (_v0) {
	var options = _v0.a.options;
	return A2(
		$elm$html$Html$textarea,
		$rundis$elm_bootstrap$Bootstrap$Form$Textarea$toAttributes(options),
		_List_Nil);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$textarea = A2($elm$core$Basics$composeL, $rundis$elm_bootstrap$Bootstrap$Form$Textarea$view, $rundis$elm_bootstrap$Bootstrap$Form$Textarea$create);
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Value = function (a) {
	return {$: 'Value', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Textarea$value = function (value_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Textarea$Value(value_);
};
var $author$project$Main$mkFormRowTextArea = F3(
	function (label, defValue, msg) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Form$Textarea$textarea(
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Textarea$value(defValue),
									$rundis$elm_bootstrap$Bootstrap$Form$Textarea$onInput(msg),
									$rundis$elm_bootstrap$Bootstrap$Form$Textarea$rows(10)
								]))
						]))
				]));
	});
var $author$project$Main$mkFormRowTextEntry = F3(
	function (label, defValue, msg) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Form$Input$text(
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Input$value(defValue),
									$rundis$elm_bootstrap$Bootstrap$Form$Input$onInput(msg)
								]))
						]))
				]));
	});
var $author$project$Main$modifyInputSpecDialog = function (model) {
	var mkLi = F2(
		function (f, val) {
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(val),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Button$button,
						_List_fromArray(
							[
								$rundis$elm_bootstrap$Bootstrap$Button$small,
								$rundis$elm_bootstrap$Bootstrap$Button$light,
								$rundis$elm_bootstrap$Bootstrap$Button$onClick(
								f(val))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Delete')
							]))
					]));
		});
	var errDiv = function () {
		var _v2 = model.inputSpecToModifyError;
		if (_v2.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('')
					]));
		} else {
			var e = _v2.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('alert alert-danger')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Error: ' + e)
					]));
		}
	}();
	var curPatterns = function (pats) {
		var _v1 = $elm$core$List$isEmpty(pats);
		if (_v1) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					mkLi($author$project$Main$DeleteInputSpecPattern),
					pats));
		}
	};
	var curChoices = function (pats) {
		var _v0 = $elm$core$List$length(pats) > 0;
		if (_v0) {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					mkLi($author$project$Main$DeleteInputSpecChoice),
					pats));
		} else {
			return $elm$html$Html$text('None');
		}
	};
	var classes = _List_fromArray(
		[$author$project$Main$InputSpecApplet, $author$project$Main$InputSpecArrayApplet, $author$project$Main$InputSpecArrayBoolean, $author$project$Main$InputSpecArrayFile, $author$project$Main$InputSpecArrayFloat, $author$project$Main$InputSpecArrayInt, $author$project$Main$InputSpecArrayRecord, $author$project$Main$InputSpecArrayString, $author$project$Main$InputSpecBool, $author$project$Main$InputSpecFile, $author$project$Main$InputSpecFloat, $author$project$Main$InputSpecHash, $author$project$Main$InputSpecInt, $author$project$Main$InputSpecRecord, $author$project$Main$InputSpecString]);
	var tbl = function (spec) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
					A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
				]),
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Grid$container,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$form,
							_List_Nil,
							_List_fromArray(
								[
									A3($author$project$Main$mkFormRowTextEntry, 'Name', spec.name, $author$project$Main$UpdateInputSpecName),
									A3($author$project$Main$mkFormRowTextEntry, 'Label', spec.label, $author$project$Main$UpdateInputSpecLabel),
									A4(
									$author$project$Main$mkFormRowSelect,
									'Class',
									A2($elm$core$List$map, $author$project$Main$inputSpecClassToString, classes),
									$author$project$Main$inputSpecClassToString(spec._class),
									$author$project$Main$UpdateInputSpecClass),
									A4($author$project$Main$mkFormRowTextEntrySubmit, 'Set Default', model.inputSpecToModifyDefault, $author$project$Main$UpdateInputSpecDefault, $author$project$Main$SaveInputSpecDefault),
									A2(
									$author$project$Main$mkFormRowText,
									'Default Value',
									$author$project$Main$defaultValueToString(spec._default)),
									A3($author$project$Main$mkFormRowTextEntry, 'Label', spec.label, $author$project$Main$UpdateInputSpecLabel),
									A4(
									$author$project$Main$mkFormRowSelect,
									'Select Pattern: ',
									_Utils_ap(
										_List_fromArray(
											['--Select--']),
										$author$project$Main$commonInputSpecPatterns),
									'',
									$author$project$Main$UpdateInputSpecAddPattern),
									A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Pattern', model.inputSpecPattern, $author$project$Main$UpdateInputSpecCustomPattern, $author$project$Main$UpdateInputSpecAddCustomPattern),
									A2(
									$author$project$Main$mkFormRowHtml,
									'Patterns',
									curPatterns(spec.patterns)),
									A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Choice', model.inputSpecChoice, $author$project$Main$UpdateInputSpecChoice, $author$project$Main$UpdateInputSpecAddChoice),
									A2(
									$author$project$Main$mkFormRowHtml,
									'Choices',
									curChoices(spec.choices)),
									A3($author$project$Main$mkFormRowTextArea, 'Help', spec.help, $author$project$Main$UpdateInputSpecHelp),
									A3($author$project$Main$mkFormRowCheckbox, 'Optional', spec.optional, $author$project$Main$UpdateInputSpecOptional)
								]))
						]))
				]));
	};
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (spec) {
				return {
					body: $elm$core$Maybe$Just(
						tbl(spec)),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$SaveInputSpec)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseInputSpecDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Add Input'))
				};
			},
			model.inputSpecToModify));
};
var $author$project$Main$paneInputSpec = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										A2($author$project$Main$ModifyInputSpecDialog, $author$project$Main$initialInputSpec, $elm$core$Maybe$Nothing))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Add Input')
									]))
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$inputSpecTable(model.app.inputSpec)
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$modifyInputSpecDialog(model)
							]))
					]))
			]));
};
var $author$project$Main$ShowJsonDialog = {$: 'ShowJsonDialog'};
var $author$project$Main$CloseJsonDialog = {$: 'CloseJsonDialog'};
var $author$project$Main$DecodeIncomingJson = {$: 'DecodeIncomingJson'};
var $author$project$Main$UpdateIncomingJson = function (a) {
	return {$: 'UpdateIncomingJson', a: a};
};
var $elm$html$Html$Attributes$cols = function (n) {
	return A2(
		_VirtualDom_attribute,
		'cols',
		$elm$core$String$fromInt(n));
};
var $author$project$Main$modifyJsonDialog = function (model) {
	var err = function () {
		var _v0 = model.jsonError;
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('')
					]));
		} else {
			var e = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('alert alert-danger')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Error: ' + e)
					]));
		}
	}();
	var body = A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
				A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
			]),
		_List_fromArray(
			[
				err,
				A2(
				$elm$html$Html$textarea,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$cols(80),
						$elm$html$Html$Attributes$rows(30),
						$elm$html$Html$Events$onInput($author$project$Main$UpdateIncomingJson)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Main$encodeApp(model.app))
					]))
			]));
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (json) {
				return {
					body: $elm$core$Maybe$Just(body),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$DecodeIncomingJson)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseJsonDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Edit JSON'))
				};
			},
			model.incomingJson));
};
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $author$project$Main$paneJson = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'text-align', 'center')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn btn-primary'),
								$elm$html$Html$Events$onClick($author$project$Main$ShowJsonDialog)
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Manual Edit')
							]))
					])),
				A2($elm$html$Html$br, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$pre,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Main$encodeApp(model.app))
					])),
				$author$project$Main$modifyJsonDialog(model)
			]));
};
var $author$project$Main$DeleteAppCategory = function (a) {
	return {$: 'DeleteAppCategory', a: a};
};
var $author$project$Main$UpdateAppAddCustomCategory = {$: 'UpdateAppAddCustomCategory'};
var $author$project$Main$UpdateAppCategories = function (a) {
	return {$: 'UpdateAppCategories', a: a};
};
var $author$project$Main$UpdateAppDescription = function (a) {
	return {$: 'UpdateAppDescription', a: a};
};
var $author$project$Main$UpdateAppDeveloperNotes = function (a) {
	return {$: 'UpdateAppDeveloperNotes', a: a};
};
var $author$project$Main$UpdateAppDxapi = function (a) {
	return {$: 'UpdateAppDxapi', a: a};
};
var $author$project$Main$UpdateAppName = function (a) {
	return {$: 'UpdateAppName', a: a};
};
var $author$project$Main$UpdateAppSummary = function (a) {
	return {$: 'UpdateAppSummary', a: a};
};
var $author$project$Main$UpdateAppTitle = function (a) {
	return {$: 'UpdateAppTitle', a: a};
};
var $author$project$Main$UpdateAppVersion = function (a) {
	return {$: 'UpdateAppVersion', a: a};
};
var $author$project$Main$UpdateCustomCategory = function (a) {
	return {$: 'UpdateCustomCategory', a: a};
};
var $author$project$Main$validCategories = _List_fromArray(
	['Annotation', 'Assembly', 'Debugging', 'Export', 'Import', 'Mappings Manipulation', 'Read Manipulation', 'Read Mapping', 'Reports', 'RNA-Seq', 'Statistics', 'Structural Variation', 'Variation Calling']);
var $author$project$Main$paneMain = function (model) {
	var mkLi = function (val) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(val),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Button$button,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Button$small,
							$rundis$elm_bootstrap$Bootstrap$Button$light,
							$rundis$elm_bootstrap$Bootstrap$Button$onClick(
							$author$project$Main$DeleteAppCategory(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Delete')
						]))
				]));
	};
	var app = model.app;
	var curCats = function () {
		var _v0 = $elm$core$List$isEmpty(app.categories);
		if (_v0) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2($elm$core$List$map, mkLi, app.categories));
		}
	}();
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Form$form,
				_List_Nil,
				_List_fromArray(
					[
						A3($author$project$Main$mkFormRowTextEntry, 'Name', app.name, $author$project$Main$UpdateAppName),
						A3($author$project$Main$mkFormRowTextEntry, 'Title', app.title, $author$project$Main$UpdateAppTitle),
						A3($author$project$Main$mkFormRowTextEntry, 'DX API Version', app.dxapi, $author$project$Main$UpdateAppDxapi),
						A3($author$project$Main$mkFormRowTextEntry, 'App Version', app.version, $author$project$Main$UpdateAppVersion),
						A3($author$project$Main$mkFormRowTextEntry, 'Summary', app.summary, $author$project$Main$UpdateAppSummary),
						A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Category', model.customCategory, $author$project$Main$UpdateCustomCategory, $author$project$Main$UpdateAppAddCustomCategory)
					])),
				A4(
				$author$project$Main$mkFormRowSelect,
				'Select Category',
				_Utils_ap(
					_List_fromArray(
						['--Select--']),
					$author$project$Main$validCategories),
				'',
				$author$project$Main$UpdateAppCategories),
				A2($author$project$Main$mkFormRowHtml, 'Categories', curCats),
				A3($author$project$Main$mkFormRowTextArea, 'Description (opt)', app.description, $author$project$Main$UpdateAppDescription),
				A3($author$project$Main$mkFormRowTextArea, 'Developer Notes (opt)', app.developerNotes, $author$project$Main$UpdateAppDeveloperNotes)
			]));
};
var $author$project$Main$ModifyOutputSpecDialog = F2(
	function (a, b) {
		return {$: 'ModifyOutputSpecDialog', a: a, b: b};
	});
var $author$project$Main$initialOutputSpec = {_class: 'file', help: '', label: 'Label', name: 'output_name', optional: false, patterns: _List_Nil};
var $author$project$Main$CloseOutputSpecDialog = {$: 'CloseOutputSpecDialog'};
var $author$project$Main$DeleteOutputSpecPattern = function (a) {
	return {$: 'DeleteOutputSpecPattern', a: a};
};
var $author$project$Main$SaveOutputSpec = {$: 'SaveOutputSpec'};
var $author$project$Main$UpdateOutputSpecAddCustomPattern = {$: 'UpdateOutputSpecAddCustomPattern'};
var $author$project$Main$UpdateOutputSpecClass = function (a) {
	return {$: 'UpdateOutputSpecClass', a: a};
};
var $author$project$Main$UpdateOutputSpecCustomPattern = function (a) {
	return {$: 'UpdateOutputSpecCustomPattern', a: a};
};
var $author$project$Main$UpdateOutputSpecName = function (a) {
	return {$: 'UpdateOutputSpecName', a: a};
};
var $author$project$Main$modifyOutputSpecDialog = function (model) {
	var mkLi = function (val) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(val),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Button$button,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Button$small,
							$rundis$elm_bootstrap$Bootstrap$Button$light,
							$rundis$elm_bootstrap$Bootstrap$Button$onClick(
							$author$project$Main$DeleteOutputSpecPattern(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Delete')
						]))
				]));
	};
	var errDiv = function () {
		var _v1 = model.outputSpecToModifyError;
		if (_v1.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('')
					]));
		} else {
			var e = _v1.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('alert alert-danger')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Error: ' + e)
					]));
		}
	}();
	var curPatterns = function (pats) {
		var _v0 = $elm$core$List$isEmpty(pats);
		if (_v0) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2($elm$core$List$map, mkLi, pats));
		}
	};
	var tbl = function (spec) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
					A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
				]),
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Grid$container,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$form,
							_List_Nil,
							_List_fromArray(
								[
									A3($author$project$Main$mkFormRowTextEntry, 'Name', spec.name, $author$project$Main$UpdateOutputSpecName),
									A4(
									$author$project$Main$mkFormRowSelect,
									'Class',
									_List_fromArray(
										['file', 'string']),
									spec._class,
									$author$project$Main$UpdateOutputSpecClass),
									A4($author$project$Main$mkFormRowTextEntrySubmit, 'Pattern', model.outputSpecPattern, $author$project$Main$UpdateOutputSpecCustomPattern, $author$project$Main$UpdateOutputSpecAddCustomPattern),
									A2(
									$author$project$Main$mkFormRowHtml,
									'Patterns',
									curPatterns(spec.patterns))
								]))
						]))
				]));
	};
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (spec) {
				return {
					body: $elm$core$Maybe$Just(
						tbl(spec)),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$SaveOutputSpec)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseOutputSpecDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Add Input'))
				};
			},
			model.outputSpecToModify));
};
var $author$project$Main$DeleteOutputSpec = function (a) {
	return {$: 'DeleteOutputSpec', a: a};
};
var $author$project$Main$SetOutputSpecToModify = function (a) {
	return {$: 'SetOutputSpecToModify', a: a};
};
var $author$project$Main$outputSpecTable = function (outputSpec) {
	var mkRow = F2(
		function (index, spec) {
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(spec.name)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(spec._class)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$primary,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$SetOutputSpecToModify(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$danger,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$DeleteOutputSpec(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var tbl = $rundis$elm_bootstrap$Bootstrap$Table$table(
		{
			options: _List_fromArray(
				[$rundis$elm_bootstrap$Bootstrap$Table$striped, $rundis$elm_bootstrap$Bootstrap$Table$hover]),
			tbody: A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tbody,
				_List_Nil,
				A2($elm$core$List$indexedMap, mkRow, outputSpec)),
			thead: $rundis$elm_bootstrap$Bootstrap$Table$simpleThead(
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Name')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Class')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							]))
					]))
		});
	var _v0 = $elm$core$List$isEmpty(outputSpec);
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No output specs')
				]));
	} else {
		return tbl;
	}
};
var $author$project$Main$paneOutputSpec = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_fromArray(
							[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										A2($author$project$Main$ModifyOutputSpecDialog, $author$project$Main$initialOutputSpec, $elm$core$Maybe$Nothing))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Add Output')
									]))
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$outputSpecTable(model.app.outputSpec)
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$modifyOutputSpecDialog(model)
							]))
					]))
			]));
};
var $author$project$Main$ModifyRegionalOptionsDialog = F3(
	function (a, b, c) {
		return {$: 'ModifyRegionalOptionsDialog', a: a, b: b, c: c};
	});
var $author$project$Main$initialRegionalOptions = {
	resources: $author$project$Main$ResourceList(_List_Nil),
	systemRequirements: $elm$core$Dict$empty
};
var $author$project$Main$CloseRegionalOptionsDialog = {$: 'CloseRegionalOptionsDialog'};
var $author$project$Main$DeleteRegionalOptionsResource = function (a) {
	return {$: 'DeleteRegionalOptionsResource', a: a};
};
var $author$project$Main$SaveRegionalOptions = {$: 'SaveRegionalOptions'};
var $author$project$Main$UpdateRegionalOptionsAddResource = {$: 'UpdateRegionalOptionsAddResource'};
var $author$project$Main$UpdateRegionalOptionsRegionName = function (a) {
	return {$: 'UpdateRegionalOptionsRegionName', a: a};
};
var $author$project$Main$UpdateRegionalOptionsResource = function (a) {
	return {$: 'UpdateRegionalOptionsResource', a: a};
};
var $author$project$Main$validRegions = _List_fromArray(
	['*', 'aws:us-east-1', 'aws:eu-central-1', 'aws:ap-southeast-2', 'aws:eu-west-2', 'aws:eu-west-2-g', 'azure:westus', 'azure:westeurope']);
var $author$project$Main$modifyRegionalOptionsDialog = function (model) {
	var mkLi = function (val) {
		return A2(
			$elm$html$Html$li,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text(val),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('btn btn-default'),
							$elm$html$Html$Events$onClick(
							$author$project$Main$DeleteRegionalOptionsResource(val))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Delete')
						]))
				]));
	};
	var curResources = function (resources) {
		var res = function () {
			if (resources.$ === 'ResourceString') {
				var s = resources.a;
				return _List_fromArray(
					[s]);
			} else {
				var vals = resources.a;
				return vals;
			}
		}();
		var _v1 = $elm$core$List$isEmpty(res);
		if (_v1) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2($elm$core$List$map, mkLi, res));
		}
	};
	var tbl = F2(
		function (regionName, options) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
								A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$form,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$table,
										_List_Nil,
										_List_fromArray(
											[
												A4($author$project$Main$mkRowSelect, 'Region Name', $author$project$Main$validRegions, regionName, $author$project$Main$UpdateRegionalOptionsRegionName),
												A4($author$project$Main$mkRowTextEntrySubmit, 'Add Resource', model.regionalOptionsResource, $author$project$Main$UpdateRegionalOptionsResource, $author$project$Main$UpdateRegionalOptionsAddResource),
												A2(
												$author$project$Main$mkRowHtml,
												'Resources',
												curResources(options.resources))
											]))
									]))
							]))
					]));
		});
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (_v0) {
				var regionName = _v0.a;
				var opts = _v0.b;
				return {
					body: $elm$core$Maybe$Just(
						A2(tbl, regionName, opts)),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$SaveRegionalOptions)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseRegionalOptionsDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Add Regional Options'))
				};
			},
			model.regionalOptionsToModify));
};
var $author$project$Main$DeleteRegionalOptions = function (a) {
	return {$: 'DeleteRegionalOptions', a: a};
};
var $author$project$Main$ModifySysReqDialog = F3(
	function (a, b, c) {
		return {$: 'ModifySysReqDialog', a: a, b: b, c: c};
	});
var $author$project$Main$SaveSysReqToRegionalOptions = function (a) {
	return {$: 'SaveSysReqToRegionalOptions', a: a};
};
var $author$project$Main$SetRegionalOptionsSysReqToModify = F3(
	function (a, b, c) {
		return {$: 'SetRegionalOptionsSysReqToModify', a: a, b: b, c: c};
	});
var $author$project$Main$SetRegionalOptionsToModify = function (a) {
	return {$: 'SetRegionalOptionsToModify', a: a};
};
var $author$project$Main$initialSystemRequirements = {clusterSpec: $elm$core$Maybe$Nothing, instanceType: 'mem2_hdd2_x2'};
var $author$project$Main$CloseSysReqDialog = {$: 'CloseSysReqDialog'};
var $author$project$Main$UpdateSysReqClusterSpecBootstrapScript = function (a) {
	return {$: 'UpdateSysReqClusterSpecBootstrapScript', a: a};
};
var $author$project$Main$UpdateSysReqClusterSpecInitialInstanceCount = function (a) {
	return {$: 'UpdateSysReqClusterSpecInitialInstanceCount', a: a};
};
var $author$project$Main$UpdateSysReqClusterSpecPorts = function (a) {
	return {$: 'UpdateSysReqClusterSpecPorts', a: a};
};
var $author$project$Main$UpdateSysReqClusterSpecType = function (a) {
	return {$: 'UpdateSysReqClusterSpecType', a: a};
};
var $author$project$Main$UpdateSysReqClusterSpecVersion = function (a) {
	return {$: 'UpdateSysReqClusterSpecVersion', a: a};
};
var $author$project$Main$UpdateSysReqEnableCluster = function (a) {
	return {$: 'UpdateSysReqEnableCluster', a: a};
};
var $author$project$Main$UpdateSysReqEntryPointName = function (a) {
	return {$: 'UpdateSysReqEntryPointName', a: a};
};
var $author$project$Main$UpdateSysReqHost = function (a) {
	return {$: 'UpdateSysReqHost', a: a};
};
var $author$project$Main$UpdateSysReqInstanceType = function (a) {
	return {$: 'UpdateSysReqInstanceType', a: a};
};
var $author$project$Main$UpdateSysReqMinCores = function (a) {
	return {$: 'UpdateSysReqMinCores', a: a};
};
var $author$project$Main$UpdateSysReqMinGpus = function (a) {
	return {$: 'UpdateSysReqMinGpus', a: a};
};
var $author$project$Main$UpdateSysReqMinMemory = function (a) {
	return {$: 'UpdateSysReqMinMemory', a: a};
};
var $elm$core$Basics$ge = _Utils_ge;
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Number = {$: 'Number'};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$number = $rundis$elm_bootstrap$Bootstrap$Form$Input$input($rundis$elm_bootstrap$Bootstrap$Form$Input$Number);
var $author$project$Main$mkFormRowNumberEntry = F3(
	function (label, defValue, msg) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Form$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						])),
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$col,
					_List_fromArray(
						[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm10]),
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Form$Input$number(
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Form$Input$value(defValue),
									$rundis$elm_bootstrap$Bootstrap$Form$Input$onInput(msg)
								]))
						]))
				]));
	});
var $author$project$Main$sysReqHostToString = function (host) {
	if (host.$ === 'Azure') {
		return 'Azure';
	} else {
		return 'AWS';
	}
};
var $author$project$Main$validClusterSpecType = _List_fromArray(
	['', 'generic', 'dxspark', 'apachespark']);
var $author$project$Main$validClusterSpecVersions = _List_fromArray(
	['', '2.4.4', '3.2.0']);
var $author$project$Main$validInstanceTypes = _List_fromArray(
	[
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 3.8, name: 'mem1_ssd1_x2', storage: 40},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 7.5, name: 'mem1_ssd1_x4', storage: 80},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 15.0, name: 'mem1_ssd1_x8', storage: 160},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 30.0, name: 'mem1_ssd1_x16', storage: 320},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 60.0, name: 'mem1_ssd1_x32', storage: 640},
		{cores: 36, gpus: 0, host: $author$project$Main$Aws, memory: 72.0, name: 'mem1_ssd1_x36', storage: 900},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 4.0, name: 'mem1_ssd1_v2_x2', storage: 50},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 8.0, name: 'mem1_ssd1_v2_x4', storage: 100},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem1_ssd1_v2_x8', storage: 200},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem1_ssd1_v2_x16', storage: 400},
		{cores: 36, gpus: 0, host: $author$project$Main$Aws, memory: 72.0, name: 'mem1_ssd1_v2_x36', storage: 900},
		{cores: 72, gpus: 0, host: $author$project$Main$Aws, memory: 144.0, name: 'mem1_ssd1_v2_x72', storage: 1800},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 3.8, name: 'mem1_ssd2_x2', storage: 160},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 7.5, name: 'mem1_ssd2_x4', storage: 320},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 15.0, name: 'mem1_ssd2_x8', storage: 640},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 30.0, name: 'mem1_ssd2_x16', storage: 1280},
		{cores: 36, gpus: 0, host: $author$project$Main$Aws, memory: 60.0, name: 'mem1_ssd2_x36', storage: 2880},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 4.0, name: 'mem1_ssd2_v2_x2', storage: 160},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 8.0, name: 'mem1_ssd2_v2_x4', storage: 320},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem1_ssd2_v2_x8', storage: 640},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem1_ssd2_v2_x16', storage: 1280},
		{cores: 36, gpus: 0, host: $author$project$Main$Aws, memory: 72.0, name: 'mem1_ssd2_v2_x36', storage: 2880},
		{cores: 72, gpus: 0, host: $author$project$Main$Aws, memory: 144.0, name: 'mem1_ssd2_v2_x72', storage: 5760},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 7.0, name: 'mem1_hdd2_x8', storage: 1680},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 60.5, name: 'mem1_hdd2_x32', storage: 3360},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 128.0, name: 'mem3_ssd1_v2_x16', storage: 600},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 256.0, name: 'mem3_ssd1_v2_x32', storage: 1200},
		{cores: 48, gpus: 0, host: $author$project$Main$Aws, memory: 384.0, name: 'mem3_ssd1_v2_x48', storage: 1800},
		{cores: 64, gpus: 0, host: $author$project$Main$Aws, memory: 512.0, name: 'mem3_ssd1_v2_x64', storage: 3200},
		{cores: 96, gpus: 0, host: $author$project$Main$Aws, memory: 768.0, name: 'mem3_ssd1_v2_x96', storage: 3600},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 30.5, name: 'mem3_ssd2_x4', storage: 800},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 61.0, name: 'mem3_ssd2_x8', storage: 1600},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 122.0, name: 'mem3_ssd2_x16', storage: 3200},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 244.0, name: 'mem3_ssd2_x32', storage: 6400},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 15.25, name: 'mem3_ssd2_v2_x2', storage: 475},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 30.5, name: 'mem3_ssd2_v2_x4', storage: 950},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 61.0, name: 'mem3_ssd2_v2_x8', storage: 1900},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 122.0, name: 'mem3_ssd2_v2_x16', storage: 3800},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 244.0, name: 'mem3_ssd2_v2_x32', storage: 7600},
		{cores: 64, gpus: 0, host: $author$project$Main$Aws, memory: 488.0, name: 'mem3_ssd2_v2_x64', storage: 15200},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem3_ssd3_x2', storage: 1250},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem3_ssd3_x4', storage: 2500},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 64.0, name: 'mem3_ssd3_x8', storage: 5000},
		{cores: 12, gpus: 0, host: $author$project$Main$Aws, memory: 96.0, name: 'mem3_ssd3_x12', storage: 7500},
		{cores: 24, gpus: 0, host: $author$project$Main$Aws, memory: 192.0, name: 'mem3_ssd3_x24', storage: 15000},
		{cores: 48, gpus: 0, host: $author$project$Main$Aws, memory: 384.0, name: 'mem3_ssd3_x48', storage: 30000},
		{cores: 96, gpus: 0, host: $author$project$Main$Aws, memory: 768.0, name: 'mem3_ssd3_x96', storage: 60000},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 17.1, name: 'mem3_hdd2_x2', storage: 420},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 34.2, name: 'mem3_hdd2_x4', storage: 850},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 68.4, name: 'mem3_hdd2_x8', storage: 1680},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem3_hdd2_v2_x2', storage: 500},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem3_hdd2_v2_x4', storage: 1000},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 64.0, name: 'mem3_hdd2_v2_x8', storage: 2000},
		{cores: 0, gpus: 0, host: $author$project$Main$Aws, memory: 0.0, name: '', storage: 0},
		{cores: 4, gpus: 0, host: $author$project$Main$Azure, memory: 7.8, name: 'azure:mem1_ssd1_x4', storage: 64},
		{cores: 8, gpus: 0, host: $author$project$Main$Azure, memory: 15.7, name: 'azure:mem1_ssd1_x8', storage: 128},
		{cores: 16, gpus: 0, host: $author$project$Main$Azure, memory: 31.4, name: 'azure:mem1_ssd1_x16', storage: 254},
		{cores: 1, gpus: 0, host: $author$project$Main$Azure, memory: 3.5, name: 'azure:mem2_ssd1_x1', storage: 128},
		{cores: 2, gpus: 0, host: $author$project$Main$Azure, memory: 7.0, name: 'azure:mem2_ssd1_x2', storage: 128},
		{cores: 4, gpus: 0, host: $author$project$Main$Azure, memory: 14.0, name: 'azure:mem2_ssd1_x4', storage: 128},
		{cores: 8, gpus: 0, host: $author$project$Main$Azure, memory: 28.0, name: 'azure:mem2_ssd1_x8', storage: 256},
		{cores: 16, gpus: 0, host: $author$project$Main$Azure, memory: 56.0, name: 'azure:mem2_ssd1_x16', storage: 512},
		{cores: 2, gpus: 0, host: $author$project$Main$Azure, memory: 14.0, name: 'azure:mem3_ssd1_x2', storage: 128},
		{cores: 4, gpus: 0, host: $author$project$Main$Azure, memory: 28.0, name: 'azure:mem3_ssd1_x4', storage: 128},
		{cores: 8, gpus: 0, host: $author$project$Main$Azure, memory: 56.0, name: 'azure:mem3_ssd1_x8', storage: 256},
		{cores: 16, gpus: 0, host: $author$project$Main$Azure, memory: 112.0, name: 'azure:mem3_ssd1_x16', storage: 512},
		{cores: 20, gpus: 0, host: $author$project$Main$Azure, memory: 140.0, name: 'azure:mem3_ssd1_x20', storage: 640},
		{cores: 2, gpus: 0, host: $author$project$Main$Azure, memory: 28.0, name: 'azure:mem4_ssd1_x2', storage: 128},
		{cores: 4, gpus: 0, host: $author$project$Main$Azure, memory: 56.0, name: 'azure:mem4_ssd1_x4', storage: 128},
		{cores: 8, gpus: 0, host: $author$project$Main$Azure, memory: 112.0, name: 'azure:mem4_ssd1_x8', storage: 256},
		{cores: 16, gpus: 0, host: $author$project$Main$Azure, memory: 224.0, name: 'azure:mem4_ssd1_x16', storage: 512},
		{cores: 32, gpus: 0, host: $author$project$Main$Azure, memory: 448.0, name: 'azure:mem4_ssd1_x32', storage: 1024},
		{cores: 64, gpus: 0, host: $author$project$Main$Azure, memory: 1792.0, name: 'azure:mem5_ssd2_x64*', storage: 8192},
		{cores: 128, gpus: 0, host: $author$project$Main$Azure, memory: 3892.0, name: 'azure:mem5_ssd2_x128*', storage: 16384},
		{cores: 8, gpus: 2, host: $author$project$Main$Aws, memory: 15.0, name: 'mem1_ssd1_gpu2_x8', storage: 60},
		{cores: 32, gpus: 4, host: $author$project$Main$Aws, memory: 60.0, name: 'mem1_ssd1_gpu2_x32', storage: 240},
		{cores: 16, gpus: 1, host: $author$project$Main$Aws, memory: 64.0, name: 'mem2_ssd1_gpu_x16', storage: 225},
		{cores: 32, gpus: 1, host: $author$project$Main$Aws, memory: 128.0, name: 'mem2_ssd1_gpu_x32', storage: 900},
		{cores: 48, gpus: 4, host: $author$project$Main$Aws, memory: 192.0, name: 'mem2_ssd1_gpu_x48', storage: 900},
		{cores: 64, gpus: 1, host: $author$project$Main$Aws, memory: 256.0, name: 'mem2_ssd1_gpu_x64', storage: 900},
		{cores: 8, gpus: 1, host: $author$project$Main$Aws, memory: 61.0, name: 'mem3_ssd1_gpu_x8', storage: 160},
		{cores: 32, gpus: 4, host: $author$project$Main$Aws, memory: 244.0, name: 'mem3_ssd1_gpu_x32', storage: 640},
		{cores: 64, gpus: 8, host: $author$project$Main$Aws, memory: 488.0, name: 'mem3_ssd1_gpu_x64', storage: 1280},
		{cores: 64, gpus: 4, host: $author$project$Main$Azure, memory: 488, name: 'azure:mem3_ssd2_gpu4_x64', storage: 2048},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 7.5, name: 'mem2_ssd1_x2', storage: 40},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 15.0, name: 'mem2_ssd1_x4', storage: 80},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 30.0, name: 'mem2_ssd1_x8', storage: 160},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 8.0, name: 'mem2_ssd1_v2_x2', storage: 75},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem2_ssd1_v2_x4', storage: 150},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem2_ssd1_v2_x8', storage: 300},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 64.0, name: 'mem2_ssd1_v2_x16', storage: 600},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 128.0, name: 'mem2_ssd1_v2_x32', storage: 1200},
		{cores: 48, gpus: 0, host: $author$project$Main$Aws, memory: 144.0, name: 'mem2_ssd1_v2_x48', storage: 1800},
		{cores: 64, gpus: 0, host: $author$project$Main$Aws, memory: 256.0, name: 'mem2_ssd1_v2_x64', storage: 2400},
		{cores: 96, gpus: 0, host: $author$project$Main$Aws, memory: 384.0, name: 'mem2_ssd1_v2_x96', storage: 3600},
		{cores: 1, gpus: 0, host: $author$project$Main$Aws, memory: 3.8, name: 'mem2_hdd2_x1', storage: 410},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 7.5, name: 'mem2_hdd2_x2', storage: 840},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 15.0, name: 'mem2_hdd2_x4', storage: 1680},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 8.0, name: 'mem2_hdd2_v2_x2', storage: 1000},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem2_hdd2_v2_x4', storage: 2000},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 15.0, name: 'mem3_ssd1_x2', storage: 40},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 30.5, name: 'mem3_ssd1_x4', storage: 80},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 61.0, name: 'mem3_ssd1_x8', storage: 160},
		{cores: 16, gpus: 0, host: $author$project$Main$Aws, memory: 122.0, name: 'mem3_ssd1_x16', storage: 320},
		{cores: 32, gpus: 0, host: $author$project$Main$Aws, memory: 244.0, name: 'mem3_ssd1_x32', storage: 640},
		{cores: 2, gpus: 0, host: $author$project$Main$Aws, memory: 16.0, name: 'mem3_ssd1_v2_x2', storage: 75},
		{cores: 4, gpus: 0, host: $author$project$Main$Aws, memory: 32.0, name: 'mem3_ssd1_v2_x4', storage: 150},
		{cores: 8, gpus: 0, host: $author$project$Main$Aws, memory: 64.0, name: 'mem3_ssd1_v2_x8', storage: 300},
		{cores: 128, gpus: 0, host: $author$project$Main$Aws, memory: 1952.0, name: 'mem4_ssd1_x128', storage: 3840}
	]);
var $author$project$Main$modifySysReqDialog = F2(
	function (model, saveFunction) {
		var hostNames = A2(
			$elm$core$List$map,
			$author$project$Main$sysReqHostToString,
			_List_fromArray(
				[$author$project$Main$Aws, $author$project$Main$Azure]));
		var curHost = A2($elm$core$Maybe$withDefault, $author$project$Main$Aws, model.editingSysReqHost);
		var instanceTypeChoices = $elm$core$List$sort(
			A2(
				$elm$core$List$map,
				function (instance) {
					return instance.name;
				},
				A2(
					$elm$core$List$filter,
					function (instance) {
						return _Utils_cmp(instance.memory, model.editingSysReqMinMemory) > -1;
					},
					A2(
						$elm$core$List$filter,
						function (instance) {
							return _Utils_cmp(instance.gpus, model.editingSysReqMinGpus) > -1;
						},
						A2(
							$elm$core$List$filter,
							function (instance) {
								return _Utils_cmp(instance.cores, model.editingSysReqMinCores) > -1;
							},
							A2(
								$elm$core$List$filter,
								function (instance) {
									return _Utils_eq(instance.host, curHost);
								},
								$author$project$Main$validInstanceTypes))))));
		var clusterForm = function (clusterSpec) {
			var clusterVersionChoices = function (spec) {
				var _v2 = spec.type_;
				if (_v2 === 'generic') {
					return _List_Nil;
				} else {
					return $author$project$Main$validClusterSpecVersions;
				}
			};
			if (clusterSpec.$ === 'Just') {
				var spec = clusterSpec.a;
				return _List_fromArray(
					[
						A4($author$project$Main$mkFormRowSelect, 'Cluster Type', $author$project$Main$validClusterSpecType, spec.type_, $author$project$Main$UpdateSysReqClusterSpecType),
						A4(
						$author$project$Main$mkFormRowSelect,
						'Cluster Version',
						clusterVersionChoices(spec),
						spec.version,
						$author$project$Main$UpdateSysReqClusterSpecVersion),
						A4(
						$author$project$Main$mkFormRowSelect,
						'Initial Instance Count',
						A2(
							$elm$core$List$map,
							$elm$core$String$fromInt,
							A2($elm$core$List$range, 1, 10)),
						$elm$core$String$fromInt(spec.initialInstanceCount),
						$author$project$Main$UpdateSysReqClusterSpecInitialInstanceCount),
						A3($author$project$Main$mkFormRowTextEntry, 'Ports', spec.ports, $author$project$Main$UpdateSysReqClusterSpecPorts),
						A3($author$project$Main$mkFormRowTextEntry, 'Bootstrap Script', spec.bootstrapScript, $author$project$Main$UpdateSysReqClusterSpecBootstrapScript)
					]);
			} else {
				return _List_Nil;
			}
		};
		var tbl = F2(
			function (entryPointName, req) {
				return A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
									A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
								]),
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$container,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$rundis$elm_bootstrap$Bootstrap$Form$form,
											_List_Nil,
											_Utils_ap(
												_List_fromArray(
													[
														A3($author$project$Main$mkFormRowTextEntry, 'Entry Point', entryPointName, $author$project$Main$UpdateSysReqEntryPointName),
														A4(
														$author$project$Main$mkFormRowSelect,
														'Host',
														hostNames,
														$author$project$Main$sysReqHostToString(curHost),
														$author$project$Main$UpdateSysReqHost),
														A3(
														$author$project$Main$mkFormRowNumberEntry,
														'Min. Cores',
														$elm$core$String$fromInt(model.editingSysReqMinCores),
														$author$project$Main$UpdateSysReqMinCores),
														A3(
														$author$project$Main$mkFormRowNumberEntry,
														'Min. Memory (GB)',
														$elm$core$String$fromInt(model.editingSysReqMinMemory),
														$author$project$Main$UpdateSysReqMinMemory),
														A3(
														$author$project$Main$mkFormRowNumberEntry,
														'GPU',
														$elm$core$String$fromInt(model.editingSysReqMinGpus),
														$author$project$Main$UpdateSysReqMinGpus),
														A4($author$project$Main$mkFormRowSelect, 'Instance Type', instanceTypeChoices, req.instanceType, $author$project$Main$UpdateSysReqInstanceType),
														A3($author$project$Main$mkFormRowCheckbox, 'Define Cluster Spec', model.enableSysReqClusterDefinition, $author$project$Main$UpdateSysReqEnableCluster)
													]),
												clusterForm(req.clusterSpec)))
										]))
								]))
						]));
			});
		return $alex_tan$elm_dialog$Dialog$view(
			A2(
				$elm$core$Maybe$map,
				function (_v0) {
					var entryPointName = _v0.a;
					var req = _v0.c;
					return {
						body: $elm$core$Maybe$Just(
							A2(tbl, entryPointName, req)),
						closeMessage: $elm$core$Maybe$Nothing,
						containerClass: $elm$core$Maybe$Nothing,
						footer: $elm$core$Maybe$Just(
							A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('btn btn-primary'),
												$elm$html$Html$Attributes$type_('button'),
												$elm$html$Html$Events$onClick(saveFunction)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Save')
											])),
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('btn btn-default'),
												$elm$html$Html$Attributes$type_('button'),
												$elm$html$Html$Events$onClick($author$project$Main$CloseSysReqDialog)
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Cancel')
											]))
									]))),
						header: $elm$core$Maybe$Just(
							$elm$html$Html$text('Add Req'))
					};
				},
				model.sysReqToModify));
	});
var $author$project$Main$regionalOptionsTable = function (model) {
	var sysReqRow = F2(
		function (regionName, _v4) {
			var entryPointName = _v4.a;
			var req = _v4.b;
			return A2(
				$elm$html$Html$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(entryPointName)
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(req.instanceType)
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('btn btn-default'),
										$elm$html$Html$Events$onClick(
										A3($author$project$Main$SetRegionalOptionsSysReqToModify, regionName, entryPointName, req))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$elm$html$Html$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('btn btn-default')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var sysReqTable = F2(
		function (regionName, systemRequirements) {
			var _v3 = $elm$core$Dict$isEmpty(systemRequirements);
			if (_v3) {
				return $elm$html$Html$text('No requirements');
			} else {
				return A2(
					$elm$html$Html$table,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('table')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$thead,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$tr,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$th,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Entry Name')
												])),
											A2(
											$elm$html$Html$th,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Resources')
												])),
											A2(
											$elm$html$Html$th,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('')
												])),
											A2(
											$elm$html$Html$th,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('')
												]))
										]))
								])),
							A2(
							$elm$html$Html$tbody,
							_List_Nil,
							A2(
								$elm$core$List$map,
								sysReqRow(regionName),
								$elm$core$Dict$toList(systemRequirements)))
						]));
			}
		});
	var regionalOptions = model.app.regionalOptions;
	var displayResources = function (res) {
		if (res.$ === 'ResourceString') {
			var s = res.a;
			return s;
		} else {
			var vals = res.a;
			return A2($elm$core$String$join, ', ', vals);
		}
	};
	var tbl = F2(
		function (regionName, opts) {
			return A2(
				$elm$html$Html$table,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('table')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$thead,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$th,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Region Name')
											])),
										A2(
										$elm$html$Html$th,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Resources')
											]))
									]))
							])),
						A2(
						$elm$html$Html$tbody,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$tr,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(regionName)
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												displayResources(opts.resources))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-default'),
														$elm$html$Html$Events$onClick(
														$author$project$Main$SetRegionalOptionsToModify(regionName))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Edit')
													]))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-default'),
														$elm$html$Html$Events$onClick(
														$author$project$Main$DeleteRegionalOptions(regionName))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Delete')
													]))
											])),
										A2(
										$elm$html$Html$td,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$button,
												_List_fromArray(
													[
														$elm$html$Html$Attributes$class('btn btn-default'),
														$elm$html$Html$Events$onClick(
														A3($author$project$Main$ModifySysReqDialog, '*', $elm$core$Maybe$Nothing, $author$project$Main$initialSystemRequirements))
													]),
												_List_fromArray(
													[
														$elm$html$Html$text('Add SysReq')
													]))
											]))
									]))
							]))
					]));
		});
	var optDiv = function (_v1) {
		var regionName = _v1.a;
		var opts = _v1.b;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(tbl, regionName, opts),
					A2(sysReqTable, regionName, opts.systemRequirements),
					A2(
					$author$project$Main$modifySysReqDialog,
					model,
					$author$project$Main$SaveSysReqToRegionalOptions(regionName))
				]));
	};
	var body = A2(
		$elm$core$List$map,
		optDiv,
		$elm$core$Dict$toList(regionalOptions));
	var _v0 = $elm$core$Dict$isEmpty(regionalOptions);
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No regional options')
				]));
	} else {
		return A2($elm$html$Html$div, _List_Nil, body);
	}
};
var $author$project$Main$paneRegionalOptions = function (model) {
	var opts = model.app.regionalOptions;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('form-group'),
				A2($elm$html$Html$Attributes$style, 'text-align', 'center')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$type_('button'),
						$elm$html$Html$Attributes$class('btn btn-default'),
						$elm$html$Html$Events$onClick(
						A3($author$project$Main$ModifyRegionalOptionsDialog, '*', $author$project$Main$initialRegionalOptions, $elm$core$Maybe$Nothing))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Add Options')
					])),
				$author$project$Main$modifyRegionalOptionsDialog(model),
				$author$project$Main$regionalOptionsTable(model)
			]));
};
var $author$project$Main$SaveSysReqToRunSpec = {$: 'SaveSysReqToRunSpec'};
var $author$project$Main$UpdateRunSpecDistribution = function (a) {
	return {$: 'UpdateRunSpecDistribution', a: a};
};
var $author$project$Main$UpdateRunSpecFile = function (a) {
	return {$: 'UpdateRunSpecFile', a: a};
};
var $author$project$Main$UpdateRunSpecInterpreter = function (a) {
	return {$: 'UpdateRunSpecInterpreter', a: a};
};
var $author$project$Main$UpdateRunSpecRelease = function (a) {
	return {$: 'UpdateRunSpecRelease', a: a};
};
var $author$project$Main$UpdateRunSpecRestartableEntryPoints = function (a) {
	return {$: 'UpdateRunSpecRestartableEntryPoints', a: a};
};
var $author$project$Main$UpdateRunSpecVersion = function (a) {
	return {$: 'UpdateRunSpecVersion', a: a};
};
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $author$project$Main$DeleteSysReq = function (a) {
	return {$: 'DeleteSysReq', a: a};
};
var $author$project$Main$SetRunSpecSysReqToModify = function (a) {
	return {$: 'SetRunSpecSysReqToModify', a: a};
};
var $author$project$Main$systemsRequirementsTable = function (reqs) {
	var mkRow = F2(
		function (index, _v1) {
			var entryPointName = _v1.a;
			var req = _v1.b;
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(entryPointName)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(req.instanceType)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$primary,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$SetRunSpecSysReqToModify(entryPointName))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$danger,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$DeleteSysReq(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var tbl = $rundis$elm_bootstrap$Bootstrap$Table$table(
		{
			options: _List_fromArray(
				[$rundis$elm_bootstrap$Bootstrap$Table$striped, $rundis$elm_bootstrap$Bootstrap$Table$hover]),
			tbody: A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tbody,
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					mkRow,
					$elm$core$Dict$toList(reqs))),
			thead: $rundis$elm_bootstrap$Bootstrap$Table$simpleThead(
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Entry Point')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Instance Type')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							]))
					]))
		});
	var _v0 = $elm$core$Dict$isEmpty(reqs);
	if (_v0) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Grid$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Grid$col,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('No requirements')
						]))
				]));
	} else {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Grid$row,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Grid$col,
					_List_Nil,
					_List_fromArray(
						[tbl]))
				]));
	}
};
var $author$project$Main$paneRunSpec = function (model) {
	var runSpec = model.app.runSpec;
	var intepreterChoices = function () {
		var _v1 = _Utils_Tuple2(runSpec.release, runSpec.version);
		if ((_v1.a === '16.04') && (_v1.b === '0')) {
			return _List_fromArray(
				[$author$project$Main$Bash, $author$project$Main$Python27]);
		} else {
			return _List_fromArray(
				[$author$project$Main$Bash, $author$project$Main$Python3]);
		}
	}();
	var appExecutionEnvVersions = function () {
		var _v0 = runSpec.release;
		if (_v0 === '20.04') {
			return _List_fromArray(
				['0']);
		} else {
			return _List_fromArray(
				['0', '1']);
		}
	}();
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Form$form,
				_List_Nil,
				_List_fromArray(
					[
						A4(
						$author$project$Main$mkFormRowSelect,
						'Interpreter',
						A2($elm$core$List$map, $author$project$Main$runSpecInterpreterToString, intepreterChoices),
						$author$project$Main$runSpecInterpreterToString(runSpec.interpreter),
						$author$project$Main$UpdateRunSpecInterpreter),
						A3($author$project$Main$mkFormRowTextEntry, 'File', runSpec.file, $author$project$Main$UpdateRunSpecFile),
						A4(
						$author$project$Main$mkFormRowSelect,
						'Distribution',
						_List_fromArray(
							['Ubuntu']),
						runSpec.distribution,
						$author$project$Main$UpdateRunSpecDistribution),
						A4(
						$author$project$Main$mkFormRowSelect,
						'Release',
						_List_fromArray(
							['20.04', '16.04']),
						runSpec.release,
						$author$project$Main$UpdateRunSpecRelease),
						A4($author$project$Main$mkFormRowSelect, 'Application Execution Version', appExecutionEnvVersions, runSpec.version, $author$project$Main$UpdateRunSpecVersion),
						A4(
						$author$project$Main$mkFormRowSelect,
						'Restartable Entry Points',
						_List_fromArray(
							['master', 'all']),
						runSpec.restartableEntryPoints,
						$author$project$Main$UpdateRunSpecRestartableEntryPoints)
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$h2,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('System Requirements')
									]))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										A3($author$project$Main$ModifySysReqDialog, '*', $elm$core$Maybe$Nothing, $author$project$Main$initialSystemRequirements))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Add SysReq')
									]))
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2($author$project$Main$modifySysReqDialog, model, $author$project$Main$SaveSysReqToRunSpec)
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$systemsRequirementsTable(model.app.runSpec.systemRequirements)
							]))
					]))
			]));
};
var $author$project$Main$ModifyTimeoutPolicyDialog = F3(
	function (a, b, c) {
		return {$: 'ModifyTimeoutPolicyDialog', a: a, b: b, c: c};
	});
var $author$project$Main$initialTimeoutPolicy = {hours: 48, minutes: 0};
var $author$project$Main$CloseTimeoutPolicyDialog = {$: 'CloseTimeoutPolicyDialog'};
var $author$project$Main$SaveTimeoutPolicy = {$: 'SaveTimeoutPolicy'};
var $author$project$Main$UpdateTimeoutPolicyEntryPointName = function (a) {
	return {$: 'UpdateTimeoutPolicyEntryPointName', a: a};
};
var $author$project$Main$UpdateTimeoutPolicyHours = function (a) {
	return {$: 'UpdateTimeoutPolicyHours', a: a};
};
var $author$project$Main$UpdateTimeoutPolicyMinutes = function (a) {
	return {$: 'UpdateTimeoutPolicyMinutes', a: a};
};
var $author$project$Main$modifyTimeoutPolicyDialog = function (model) {
	var tbl = F2(
		function (entryPointName, policy) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'overflow-y', 'auto'),
						A2($elm$html$Html$Attributes$style, 'max-height', '60vh')
					]),
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$container,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Form$form,
								_List_Nil,
								_List_fromArray(
									[
										A3($author$project$Main$mkFormRowTextEntry, 'Entry Point Name', entryPointName, $author$project$Main$UpdateTimeoutPolicyEntryPointName),
										A3(
										$author$project$Main$mkFormRowTextEntry,
										'Hours',
										$elm$core$String$fromInt(policy.hours),
										$author$project$Main$UpdateTimeoutPolicyHours),
										A3(
										$author$project$Main$mkFormRowTextEntry,
										'Minutes',
										$elm$core$String$fromInt(policy.minutes),
										$author$project$Main$UpdateTimeoutPolicyMinutes)
									]))
							]))
					]));
		});
	return $alex_tan$elm_dialog$Dialog$view(
		A2(
			$elm$core$Maybe$map,
			function (_v0) {
				var entryPointName = _v0.a;
				var policy = _v0.b;
				return {
					body: $elm$core$Maybe$Just(
						A2(tbl, entryPointName, policy)),
					closeMessage: $elm$core$Maybe$Nothing,
					containerClass: $elm$core$Maybe$Nothing,
					footer: $elm$core$Maybe$Just(
						A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-primary'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$SaveTimeoutPolicy)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Save')
										])),
									A2(
									$elm$html$Html$button,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('btn btn-default'),
											$elm$html$Html$Attributes$type_('button'),
											$elm$html$Html$Events$onClick($author$project$Main$CloseTimeoutPolicyDialog)
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Cancel')
										]))
								]))),
					header: $elm$core$Maybe$Just(
						$elm$html$Html$text('Add Policy'))
				};
			},
			model.timeoutPolicyToModify));
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Secondary = {$: 'Secondary'};
var $rundis$elm_bootstrap$Bootstrap$Button$secondary = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled($rundis$elm_bootstrap$Bootstrap$Internal$Button$Secondary));
var $author$project$Main$DeleteTimeoutPolicy = function (a) {
	return {$: 'DeleteTimeoutPolicy', a: a};
};
var $author$project$Main$SetTimeoutPolicyToModify = function (a) {
	return {$: 'SetTimeoutPolicyToModify', a: a};
};
var $author$project$Main$timeoutPolicyTable = function (policies) {
	var mkRow = F2(
		function (index, _v1) {
			var entryPointName = _v1.a;
			var policy = _v1.b;
			return A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tr,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(entryPointName)
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(policy.hours))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								$elm$core$String$fromInt(policy.minutes))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$primary,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$SetTimeoutPolicyToModify(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Edit')
									]))
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$td,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$danger,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										$author$project$Main$DeleteTimeoutPolicy(index))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Delete')
									]))
							]))
					]));
		});
	var tbl = $rundis$elm_bootstrap$Bootstrap$Table$table(
		{
			options: _List_fromArray(
				[$rundis$elm_bootstrap$Bootstrap$Table$striped, $rundis$elm_bootstrap$Bootstrap$Table$hover]),
			tbody: A2(
				$rundis$elm_bootstrap$Bootstrap$Table$tbody,
				_List_Nil,
				A2(
					$elm$core$List$indexedMap,
					mkRow,
					$elm$core$Dict$toList(policies))),
			thead: $rundis$elm_bootstrap$Bootstrap$Table$simpleThead(
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Entry Point')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Hours')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Minutes')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Table$th,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							]))
					]))
		});
	var _v0 = $elm$core$Dict$isEmpty(policies);
	if (_v0) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('No timeout policies')
				]));
	} else {
		return tbl;
	}
};
var $author$project$Main$paneTimeoutPolicy = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Button$button,
								_List_fromArray(
									[
										$rundis$elm_bootstrap$Bootstrap$Button$secondary,
										$rundis$elm_bootstrap$Bootstrap$Button$onClick(
										A3($author$project$Main$ModifyTimeoutPolicyDialog, '*', $author$project$Main$initialTimeoutPolicy, $elm$core$Maybe$Nothing))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Add Timeout')
									]))
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$timeoutPolicyTable(model.app.runSpec.timeoutPolicy)
							]))
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$row,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$col,
						_List_Nil,
						_List_fromArray(
							[
								$author$project$Main$modifyTimeoutPolicyDialog(model)
							]))
					]))
			]));
};
var $author$project$Main$DeleteAuthorizedUser = function (a) {
	return {$: 'DeleteAuthorizedUser', a: a};
};
var $author$project$Main$DeleteDeveloper = function (a) {
	return {$: 'DeleteDeveloper', a: a};
};
var $author$project$Main$UpdateAddAuthorizedUser = {$: 'UpdateAddAuthorizedUser'};
var $author$project$Main$UpdateAddDeveloper = {$: 'UpdateAddDeveloper'};
var $author$project$Main$UpdateAuthorizedUser = function (a) {
	return {$: 'UpdateAuthorizedUser', a: a};
};
var $author$project$Main$UpdateDeveloper = function (a) {
	return {$: 'UpdateDeveloper', a: a};
};
var $author$project$Main$paneUsers = function (model) {
	var mkLi = F2(
		function (f, val) {
			return A2(
				$elm$html$Html$li,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(val),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Button$button,
						_List_fromArray(
							[
								$rundis$elm_bootstrap$Bootstrap$Button$small,
								$rundis$elm_bootstrap$Bootstrap$Button$light,
								$rundis$elm_bootstrap$Bootstrap$Button$onClick(
								f(val))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Delete')
							]))
					]));
		});
	var developers = model.app.developers;
	var curDevelopers = function () {
		var _v1 = $elm$core$List$isEmpty(developers);
		if (_v1) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					mkLi($author$project$Main$DeleteDeveloper),
					developers));
		}
	}();
	var authorizedUsers = model.app.authorizedUsers;
	var curAuthorizedUsers = function () {
		var _v0 = $elm$core$List$isEmpty(authorizedUsers);
		if (_v0) {
			return $elm$html$Html$text('None');
		} else {
			return A2(
				$elm$html$Html$ul,
				_List_Nil,
				A2(
					$elm$core$List$map,
					mkLi($author$project$Main$DeleteAuthorizedUser),
					authorizedUsers));
		}
	}();
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Grid$container,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Form$form,
				_List_Nil,
				_List_fromArray(
					[
						A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Developer', model.developer, $author$project$Main$UpdateDeveloper, $author$project$Main$UpdateAddDeveloper),
						A2($author$project$Main$mkFormRowHtml, 'Developers', curDevelopers),
						A4($author$project$Main$mkFormRowTextEntrySubmit, 'Add Authorized User', model.authorizedUser, $author$project$Main$UpdateAuthorizedUser, $author$project$Main$UpdateAddAuthorizedUser),
						A2($author$project$Main$mkFormRowHtml, 'Authorized Users', curAuthorizedUsers)
					]))
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Tab$Right = {$: 'Right'};
var $rundis$elm_bootstrap$Bootstrap$Tab$layout = F2(
	function (layout_, _v0) {
		var configRec = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Tab$Config(
			_Utils_update(
				configRec,
				{
					layout: $elm$core$Maybe$Just(layout_)
				}));
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$right = $rundis$elm_bootstrap$Bootstrap$Tab$layout($rundis$elm_bootstrap$Bootstrap$Tab$Right);
var $rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem = F2(
	function (_v0, configRec) {
		var activeTab = _v0.a.activeTab;
		if (activeTab.$ === 'Nothing') {
			return $elm$core$List$head(configRec.items);
		} else {
			var id = activeTab.a;
			return function (found) {
				if (found.$ === 'Just') {
					var f = found.a;
					return $elm$core$Maybe$Just(f);
				} else {
					return $elm$core$List$head(configRec.items);
				}
			}(
				$elm$core$List$head(
					A2(
						$elm$core$List$filter,
						function (_v2) {
							var item_ = _v2.a;
							return _Utils_eq(item_.id, id);
						},
						configRec.items)));
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$Hidden = {$: 'Hidden'};
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $rundis$elm_bootstrap$Bootstrap$Tab$Start = {$: 'Start'};
var $rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _v0 = _Utils_Tuple2(withAnimation_, visibility);
		_v0$2:
		while (true) {
			if (_v0.a) {
				switch (_v0.b.$) {
					case 'Hidden':
						var _v1 = _v0.b;
						return $rundis$elm_bootstrap$Bootstrap$Tab$Start;
					case 'Start':
						var _v2 = _v0.b;
						return $rundis$elm_bootstrap$Bootstrap$Tab$Showing;
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return $rundis$elm_bootstrap$Bootstrap$Tab$Showing;
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$renderLink = F4(
	function (id, active, _v0, configRec) {
		var attributes = _v0.a.attributes;
		var children = _v0.a.children;
		var commonClasses = _List_fromArray(
			[
				_Utils_Tuple2('nav-link', true),
				_Utils_Tuple2('active', active)
			]);
		var clickHandler = $elm$html$Html$Events$onClick(
			configRec.toMsg(
				$rundis$elm_bootstrap$Bootstrap$Tab$State(
					{
						activeTab: $elm$core$Maybe$Just(id),
						visibility: A2($rundis$elm_bootstrap$Bootstrap$Tab$visibilityTransition, configRec.withAnimation && (!active), $rundis$elm_bootstrap$Bootstrap$Tab$Hidden)
					})));
		var linkItem = configRec.useHash ? A2(
			$elm$html$Html$a,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(commonClasses),
						clickHandler,
						$elm$html$Html$Attributes$href('#' + id)
					]),
				attributes),
			children) : A2(
			$elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$classList(
						_Utils_ap(
							commonClasses,
							_List_fromArray(
								[
									_Utils_Tuple2('btn', true),
									_Utils_Tuple2('btn-link', true)
								]))),
						clickHandler
					]),
				attributes),
			children);
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('nav-item')
				]),
			_List_fromArray(
				[linkItem]));
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles = function (opacity) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$Attributes$style,
			'opacity',
			$elm$core$String$fromInt(opacity)),
			A2($elm$html$Html$Attributes$style, '-webkit-transition', 'opacity 0.15s linear'),
			A2($elm$html$Html$Attributes$style, '-o-transition', 'opacity 0.15s linear'),
			A2($elm$html$Html$Attributes$style, 'transition', 'opacity 0.15s linear')
		]);
};
var $rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes = F2(
	function (_v0, configRec) {
		var visibility = _v0.a.visibility;
		switch (visibility.$) {
			case 'Hidden':
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'none')
					]);
			case 'Start':
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						A2($elm$html$Html$Attributes$style, 'opacity', '0')
					]);
			default:
				return _Utils_ap(
					_List_fromArray(
						[
							A2($elm$html$Html$Attributes$style, 'display', 'block')
						]),
					$rundis$elm_bootstrap$Bootstrap$Tab$transitionStyles(1));
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane = F5(
	function (id, active, _v0, state, configRec) {
		var attributes = _v0.a.attributes;
		var children = _v0.a.children;
		var displayAttrs = active ? A2($rundis$elm_bootstrap$Bootstrap$Tab$activeTabAttributes, state, configRec) : _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'none')
			]);
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id(id),
						$elm$html$Html$Attributes$class('tab-pane')
					]),
				_Utils_ap(displayAttrs, attributes)),
			children);
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes = function (configRec) {
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('nav', true),
						_Utils_Tuple2('nav-tabs', !configRec.isPill),
						_Utils_Tuple2('nav-pills', configRec.isPill)
					]))
			]),
		_Utils_ap(
			function () {
				var _v0 = configRec.layout;
				if (_v0.$ === 'Just') {
					switch (_v0.a.$) {
						case 'Justified':
							var _v1 = _v0.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class('nav-justified')
								]);
						case 'Fill':
							var _v2 = _v0.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class('nav-fill')
								]);
						case 'Center':
							var _v3 = _v0.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class('justify-content-center')
								]);
						default:
							var _v4 = _v0.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class('justify-content-end')
								]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			configRec.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Tab$view = F2(
	function (state, _v0) {
		var configRec = _v0.a;
		var _v1 = A2($rundis$elm_bootstrap$Bootstrap$Tab$getActiveItem, state, configRec);
		if (_v1.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$ul,
						$rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						_List_Nil),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tab-content')
							]),
						_List_Nil)
					]));
		} else {
			var currentItem = _v1.a.a;
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$ul,
						$rundis$elm_bootstrap$Bootstrap$Tab$tabAttributes(configRec),
						A2(
							$elm$core$List$map,
							function (_v2) {
								var item_ = _v2.a;
								return A4(
									$rundis$elm_bootstrap$Bootstrap$Tab$renderLink,
									item_.id,
									_Utils_eq(item_.id, currentItem.id),
									item_.link,
									configRec);
							},
							configRec.items)),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tab-content')
							]),
						A2(
							$elm$core$List$map,
							function (_v3) {
								var item_ = _v3.a;
								return A5(
									$rundis$elm_bootstrap$Bootstrap$Tab$renderTabPane,
									item_.id,
									_Utils_eq(item_.id, currentItem.id),
									item_.pane,
									state,
									configRec);
							},
							configRec.items))
					]));
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Tab$withAnimation = function (_v0) {
	var configRec = _v0.a;
	return $rundis$elm_bootstrap$Bootstrap$Tab$Config(
		_Utils_update(
			configRec,
			{withAnimation: true}));
};
var $author$project$Main$view = function (model) {
	var title = 'The Appetizer';
	var err = function () {
		var _v0 = model.error;
		if (_v0.$ === 'Nothing') {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('')
					]));
		} else {
			var e = _v0.a;
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('alert alert-danger')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Error: ' + e)
					]));
		}
	}();
	return {
		body: _List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$container,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h1,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(title)
							])),
						err,
						A2(
						$rundis$elm_bootstrap$Bootstrap$Tab$view,
						model.tabState,
						A2(
							$rundis$elm_bootstrap$Bootstrap$Tab$items,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabMain',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Main')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneMain(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabRunSpec',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('RunSpec')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneRunSpec(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabTimeoutPolicy',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Timeout')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneTimeoutPolicy(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabExecDepends',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('ExecDepends')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneExecDepends(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabInputSpec',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Input')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneInputSpec(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabOutputSpec',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Output')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneOutputSpec(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabAccessSpec',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Access')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneAccessSpec(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabUsers',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Users')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneUsers(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabHttpsApp',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('HttpsApp')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneHttpsApp(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabRegionalOptions',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('RegOpts')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneRegionalOptions(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabDebug',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Debug')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneDebug(model)
												]))
									}),
									$rundis$elm_bootstrap$Bootstrap$Tab$item(
									{
										id: 'tabJson',
										link: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$link,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('JSON')
												])),
										pane: A2(
											$rundis$elm_bootstrap$Bootstrap$Tab$pane,
											_List_Nil,
											_List_fromArray(
												[
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													$author$project$Main$paneJson(model)
												]))
									})
								]),
							$rundis$elm_bootstrap$Bootstrap$Tab$right(
								$rundis$elm_bootstrap$Bootstrap$Tab$withAnimation(
									$rundis$elm_bootstrap$Bootstrap$Tab$config($author$project$Main$TabMsg)))))
					]))
			]),
		title: title
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$UrlChanged, onUrlRequest: $author$project$Main$LinkClicked, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));