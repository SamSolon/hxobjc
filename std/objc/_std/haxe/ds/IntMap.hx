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
//@:category("NSMutableDictionary")
@:coreApi
class IntMap<T> implements Map.IMap<Int,T> 
{
	private var _map:NSMutableDictionary;
 	
 	public function new() : Void {
 		_map = new NSMutableDictionary();
 	}

 	public function set( key : Int, value : T ) : Void {
 		//untyped this.setObject (value, key);
 		//untyped __objc__("[self setObject:[NSString stringWithFormat:@\"%i\", value] forKey:[NSString stringWithFormat:@\"%i\",key]]");
 		_map.setObject(value, key);
 	}

 	public function get( key : Int ) : Null<T> {
 		//return untyped this.objectForKey ( key );
 		//return untyped __objc__("[self objectForKey:[NSString stringWithFormat:@\"%i\",key]]");
 		return _map.objectForKey(key);
 	}

 	public function exists( key : Int ) : Bool {
 		//return untyped this.objectForKey ( key ) != null;
 		//return untyped __objc__("[self objectForKey:[NSString stringWithFormat:@\"%i\",key]] != nil");
 		return get(key) != null;
 	}

 	public function remove( key : Int ) : Bool {
 		//return untyped this.removeObjectForKey ( key );
 		if (exists(key)) {
 			//untyped __objc__("[self removeObjectForKey:[NSString stringWithFormat:@\"%i\",key]]");
 			_map.removeObjectForKey(key);
 			return true;
 		}
 		return false;
 	}

 	public function keys() : Iterator<Int> {
 	//TODO: Fix this!!!!
 		var a:Array<Int> = untyped _map.allKeys();
 		return a.iterator();
 	}

 	public function iterator() : Iterator<T> {
 	//TODO: Fix this!!!!
 		var a:Array<Dynamic> = _map.allValues();
 		return a.iterator();
 	}

 	public function toString() : String {
 		return _map.description();
 	}

}
