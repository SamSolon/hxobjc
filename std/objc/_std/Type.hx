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

enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum<Dynamic> );
	TUnknown;
}

@:include("objc/runtime.h")
@:coreApi
class Type {
	public static function getClass<T>( o : T ) : Class<T> untyped {
		return null;//return untyped __objc__("[o class]");
//		if (o==null || !Reflect.isObject(o))  return null;
//		var c = o.__class();
//		switch(c.toString()){
//			case "__Anon" : return null;
//			case "Class" : return null;
//		}
//		return c;
	}

	public static function getEnum( o : EnumValue ) : Enum<Dynamic> untyped {
		if (o==null) return null;
		return untyped o.__class();
	}


	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> {
		if( c == null )
			return null;
		return untyped c.superclass();//__objc__("class_getSuperclass(c);");
	}

	public static function getClassName( c : Class<Dynamic> ) : String {
		if( c == null )
			return null;
		return untyped __objc__("NSStringFromClass([c class])");
	}

	public static function getEnumName( e : Enum<Dynamic> ) : String {
		return untyped e.__ToString();
	}

	public static function resolveClass( name : String ) : Class<Dynamic> {
		var result:Class<Dynamic> = untyped __objc__("NSClassFromString ( name )");
/*		if (result!=null && result.__IsEnum() )
			return null;*/
		return result;
	}

	public static function resolveEnum( name : String ) : Enum<Dynamic> untyped {
		var result:Class<Dynamic> = Class.Resolve(name);
		if (result!=null && !result.__IsEnum() )
			return null;
		return result;
	}

	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T {
		if (cl!=null)
			return untyped __objc__("[[cl alloc] initFromArray:args]");
		return null;
	}

	public static function createEmptyInstance<T>( cl : Class<T> ) : T {
		return null;//untyped __objc__("[cl alloc]");
	}

	public static function createEnum<T>( e : Enum<T>, constr : String, ?params : Array<Dynamic> ) : T {
		if (untyped e.mConstructEnum != null)
			return untyped e.mConstructEnum(constr,params);
		return null;
	}

	public static function createEnumIndex<T>( e : Enum<T>, index : Int, ?params : Array<Dynamic> ) : T {
		var c = Type.getEnumConstructs(e)[index];
		if( c == null ) throw index+" is not a valid enum constructor index";
		return createEnum(e,c,params);
	}

	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		return untyped c.GetInstanceFields();
	}

	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		return untyped c.GetClassFields();
	}

	public static function getEnumConstructs( e : Enum<Dynamic> ) : Array<String> untyped {
		return untyped e.GetClassFields();
	}

	public static function typeof( v : Dynamic ) : ValueType untyped {
/*		http://stackoverflow.com/questions/2518761/get-type-of-nsnumber*/
		if (v==null) return TNull;

		if (untyped __objc__("[v isKindOfClass:[NSValue class]]")) {
			if (untyped __objc__("strstr(\"islqISLQ\", [v objCType]) != NULL")) {return TInt;}
			else if (untyped __objc__("strstr(\"fd\", [v objCType]) != NULL")) {return TFloat;}
		}
		
		if (untyped __objc__("class_isMetaClass(object_getClass(v))")) { return TClass; }
		
//		if (v.isKindOfClass ( Bool.__class() )) return TBool;
//		else if (v.isKindOfClass ( Int.__class() )) return TInt;
//		else if (v.isKindOfClass ( Float.__class() )) return TFloat;
//		else if (v.isKindOfClass ( TFunction.__class() )) return TFunction;
//		else if (v.isKindOfClass ( TObject.__class() )) return TObject;
//		else if (v.isKindOfClass ( Int.__class() )) return TEnum ( v.__class() );
		//return TClass ( v.__class() );

		return TNull;
	}

	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		return a==b;
	}

	public static function enumConstructor( e : EnumValue ) : String {
		return untyped e.__Tag();
	}

	public static function enumParameters( e : EnumValue ) : Array<Dynamic> {
		var result : Array<Dynamic> =  untyped e.__EnumParams();
		return result==null ? [] : result;
	}

	public inline static function enumIndex( e : EnumValue ) : Int {
		return cast(untyped e.__Index(), Int);
	}

	public static function allEnums<T>( e : Enum<T> ) : Array<T> {
		var names:Array<String> =  untyped e.GetClassFields();
		var enums = new Array<T>();
		for(name in names){
			try {
				var result:T = untyped e.mConstructEnum(name,null);
				enums.push( result );
			} catch ( invalidArgCount:String) {
			}
		}
		return enums;
	}
}

