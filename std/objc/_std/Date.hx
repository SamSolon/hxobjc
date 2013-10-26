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

import objc.foundation.NSDate;
import objc.foundation.NSCalendar;
typedef NSDateComponents = Dynamic;

@:framework("Foundation") @:category("NSDate") @:coreApi class Date {

//	private var _seconds :Float;
//	private var _calendar :NSCalendar;
//	private var _components :NSDateComponents;

	public function new (year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) :Void {
		
		var calendar = NSCalendar.currentCalendar();
		// This is an Int enum in objc
		var components = calendar.components (untyped NSYearCalendarUnit | 
											NSMonthCalendarUnit | 
											NSDayCalendarUnit | 
											NSHourCalendarUnit | 
											NSMinuteCalendarUnit | 
											NSSecondCalendarUnit, NSDate.date());
		components.setYear ( year );
		components.setMonth ( month );
		components.setDay ( day );
		components.setHour ( hour );
		components.setMinute ( min );
		components.setSecond ( sec );
		
		untyped __objc__("self = [self.calendar dateFromComponents:components]");
	}

	public function getTime() : Float {
		return untyped __objc__("[self timeIntervalSince1970]") * 1000.0;
	}

	public function getHours() : Int { 
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSHourCalendarUnit fromDate:self] hour]"); 
	}

	public function getMinutes() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSMinuteCalendarUnit fromDate:self] minute]"); 	
	}

	public function getSeconds() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSSecondCalendarUnit fromDate:self] second]"); 	
	}

	public function getFullYear() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSYearCalendarUnit fromDate:self] year]"); 	
	}

	public function getMonth() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSMonthCalendarUnit fromDate:self] month]"); 
	}

	public function getDate() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSDayCalendarUnit fromDate:self] day]"); 
	}

	public function getDay() : Int {
		return untyped __objc__("[[[NSCalendar currentCalendar] components:NSWeekdayCalendarUnit fromDate:self] weekday]"); 
	}

	public function toString():String { 
		return untyped __objc__("[self description]");
	}

	public static function now() : Date {
		return untyped __objc__("[NSDate date]");
	}

	public static function fromTime( t : Float ) : Date {
		return untyped __objc__("[NSDate dateWithTimeIntervalSince1970:t]");
	}

	public static function fromString( s : String ) : Date {
		switch( s.length ) {
			case 8: // hh:mm:ss
				//TODO: the compiler needs clues on the type of the array, otherwise the casting is returning NSMutableArray. Check why.
				var k :Array<String> = s.split(":");
				var d : Date = new Date(0,0,0,Std.parseInt(k[0]),Std.parseInt(k[1]),Std.parseInt(k[2]));
				return d;
			case 10: // YYYY-MM-DD
				var k :Array<String> = s.split("-");
				return new Date(Std.parseInt(k[0]),Std.parseInt(k[1])-1,Std.parseInt(k[2]),0,0,0);
			case 19: // YYYY-MM-DD hh:mm:ss
				var k :Array<String> = s.split(" ");
				var y = k[0].split("-");
				var t = k[1].split(":");
				return new Date(Std.parseInt(y[0]),Std.parseInt(y[1]) - 1,Std.parseInt(y[2]),
					Std.parseInt(t[0]),Std.parseInt(t[1]),Std.parseInt(t[2]));
			default:
				throw "Invalid date format : " + s;
		}
		return null;
	}
}

