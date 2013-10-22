/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package haxe.ds;

import objc.foundation.NSDictionary;
	
@:framework("Foundation")
@:category("NSMutableDictionary")
@:coreApi
class StringMap<T> implements Map.IMap<String,T> {
	
	public function new() : Void {
		
	}

	public function set( key : String, value : T ) : Void {
		//untyped this.setObject (value, key);
		untyped __objc__("[self setObject:value forKey:key]");
	}

	public function get( key : String ) : Null<T> {
		return untyped this.objectForKey ( key );
	}

	public function exists( key : String ) : Bool {
		return untyped this.objectForKey ( key ) != null;
	}

	public function remove( key : String ) : Bool {
		if (exists(key)) {
			untyped this.removeObjectForKey ( key );
			return true;
		}
		return false;
	}

	/**
		Returns an iterator of all keys in the hashtable.
	**/
	public function keys() : Iterator<String> {
		var a:Array<String> = untyped __objc__("[NSMutableArray arrayWithArray:[self allKeys]]");
		return a.iterator();
	}

	/**
		Returns an iterator of all values in the hashtable.
	**/
	public function iterator() : Iterator<T> {
		var a:Array<String> = untyped this.allValues();
		var it = a.iterator();
		var me = this;
		return untyped {
			hasNext : function() { return it.hasNext(); },
			next : function() { return me.__Internal.__Field(it.next(),true); }
		};
	}

	/**
		Returns an displayable representation of the hashtable content.
	**/

	public function toString() : String {
		return untyped this.description();
/*		var s = new StringBuf();
		s.add("{");
		var it = keys();
		for( i in it ) {
			s.add(i);
			s.add(" => ");
			s.add(Std.string(get(i)));
			if( it.hasNext() )
				s.add(", ");
		}
		s.add("}");
		return s.toString();*/
	}
}
