/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

// @:category meta will transform the Array class into a category of the NSMutableArray
// So Array is actually a NSMutableArray and you can call any native method if you use untyped

@:category("NSMutableArray")
@:coreApi
@:final
class Array<T> {

	@:getterBody("return [self count];")
	public var length (default, null) : Int;


	public function new () : Void {
		
	}

	public function concat (a : Array<T>) : Array<T> {
		// Be careful with NSArray and NSMutableArray, they are not compatible in some cases
		var b :Array<T> = new Array<T>();
		untyped b.addObjectsFromArray ( this );
		untyped b.addObjectsFromArray ( a );
		return b;
	}

	public function copy () : Array<T> {
		return untyped Array.arrayWithArray ( this );
	}

	public function insert ( pos : Int, x : T ) : Void {
		//trace("insert");
		//untyped this.insertObject (x, pos);
		untyped __objc__("[self insertObject:(x!=nil?x:[NSNull null]) atIndex:pos]");
	}

	public function join ( sep : String ) : String {
		return untyped NSMutableString.stringWithString ( this.componentsJoinedByString( sep ));
	}

	public function toString () : String {
		return untyped NSMutableString.stringWithString ( this.description());//"[" + untyped this.componentsJoinedByString(",") + "]";
	}

	public function pop () : Null<T> {
		//trace("pop");
		if( this.length == 0 )
			return null;
		var theLastObject :T = untyped __objc__("[self lastObject]");
		untyped __objc__("if ([theLastObject isKindOfClass:[NSNull class]]) theLastObject = nil");
		untyped __objc__("[self removeLastObject]");
		return theLastObject;
	}

	public function push (x:T) : Int {
		untyped __objc__("[self addObject:(x!=nil?x:[NSNull null])]");
		return this.length;
	}

	public function unshift (x : T) : Void {
		untyped __objc__("[self insertObject:(x!=nil?x:[NSNull null]) atIndex:0]");
	}

	public function remove (x : T) : Bool {
		var containsObject :Bool = untyped this.containsObject ( x );
		if (containsObject) {
			untyped this.removeObject ( x );
		}
		return containsObject;
	}

	public function reverse () : Void {
		var reverseArray = untyped this.reverseObjectEnumerator().allObjects();
/*		NSMutableArray * reverseArray = [NSMutableArray arrayWithCapacity:[self count]]; 

		for (id element in [myArray reverseObjectEnumerator]) {
		    [reverseArray addObject:element];
		}*/
	}

	public function shift () : Null<T> {
		if (this.length > 0) {
			var obj = untyped this.objectAtIndex ( 0 );
			untyped this.removeObjectAtIndex ( 0 );
			return obj;
		}
		return null;
	}

	public function slice( pos : Int, ?end : Int ) : Array<T> {
		return splice (pos, end-pos);
	}

	public function sort(f:T->T->Int) : Void {
		
	}

	public function splice( pos : Int, len : Int ) : Array<T> {
		//var newArray :Array<T> = null;
		untyped __objc__("NSArray *newArray = [self subarrayWithRange:NSMakeRange(pos, len)]");
/*		untyped this.subarrayWithRange ( new NSRange (pos, len) );*/
		untyped this.removeObjectsInArray ( untyped newArray );
/*		nativeArray.removeObjectsInRange ( new NSRange (pos, len));*/

		return untyped Array.arrayWithArray ( untyped newArray );
	}
	
	public function iterator () : Iterator<T> {
		var it = new HxIterator<T>();
		it.arr = this;
		it.len = length;
		return it;
		
/*		untyped __objc__("__block int p = 0;
	
	return [NSMutableDictionary dictionaryWithObjectsAndKeys:
			[^BOOL() { return p < [self count]; } copy], @\"hasNext\",
			[^id() { id i = [self objectAtIndex:p]; p += 1; return i; } copy], @\"next\",
			nil]");
		return null;*/
/*		var i = 0;
		var len = length;
		
		return {
			hasNext : function() {
				return i < len;
			},
			next : function() {
				return this[i++];
			}
		};*/
	}

	public function map<S>( f : T -> S ) : Array<S> {
/*		var ret = [];
		for (elt in this)
			ret.push(f(elt));
		return ret;*/
		return null;
	}

	public function filter( f : T -> Bool ) : Array<T> {
/*		var ret = [];
		for (elt in this)
			if (f(elt))
				ret.push(elt);
		return ret;*/
		return null;
	}
	
	
	function hx_replaceObjectAtIndex (index:Int, withObject:Dynamic) :Void {
		//trace("safeReplaceObjectAtIndex");
		untyped __objc__("if (index >= [self count]) while ([self count] <= index) [self addObject:[NSNull null]]");
		untyped __objc__("[self replaceObjectAtIndex:index withObject:(withObject==nil?[NSNull null]:withObject)]");
	}
	function hx_objectAtIndex (index:Int) :Dynamic {
/*		TODO: this trace is generated in a TLazy*/
/*		trace("safeObjectAtIndex "+NSNumber.numberWithInt(index));*/
		untyped __objc__("if (index >= [self count]) while ([self count] <= index) [self addObject:[NSNull null]]");
		var obj :Dynamic = untyped this.objectAtIndex(index);
		untyped __objc__("if ([obj isKindOfClass:[NSNull class]]) obj = nil");
		return obj;
	}
}

