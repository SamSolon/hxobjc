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

import objc.foundation.NSObject;

@:coreApi class Std {
	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		var c = t.__class();
		
		if (untyped __objc__("[v isKindOfClass:c]")) {
			return true;
		}
		
		// Assume its a protocol (is there a way to check for this directly?)
		return untyped __objc__("[[v class] conformsToProtocol:t]");
	}
	
	public static function instance<T>( v : { }, c : Class<T> ) : T {
		return Std.is(v, c) ? cast v : null;
	}
	
	public static function string( s : Dynamic ) : String {
		return if (s == null) "null" else untyped __objc__("[s description]");
	}

	public static function int( x : Float ) : Int {
		return cast (x, Int);
	}

	public static function parseInt( x : String ) : Null<Int> {
		return untyped x.intValue();
	}

	public static function parseFloat( x : String ) : Float {
		return untyped __objc__("[x floatValue]");
	}

	public static function random( x : Int ) : Int {
		if (x <= 0) return 0;
		return untyped __objc__("rand() % x");
	}
}
