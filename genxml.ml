(*
 * Copyright (C)2005-2013 Haxe Foundation
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
 *)

open Ast
open Type
open Common

type xml =
	| Node of string * (string * string) list * xml list
	| PCData of string
	| CData of string

let tag name = Node (name,[],[])
let xml name att = Node (name,att,[])
let node name att childs = Node (name,att,childs)
let pcdata s = PCData s
let cdata s = CData s

let pmap f m =
	PMap.fold (fun x acc -> f x :: acc) m []

let gen_path (p,n) priv =
	("path",String.concat "." (p @ [n]))

let gen_string s =
	if String.contains s '<' || String.contains s '>' || String.contains s '&' then	cdata s else pcdata s

let gen_doc s =
	(* remove trailing space and convert newlines *)
	let s = ExtString.String.strip s in
	let s = String.concat "\n" (ExtString.String.nsplit (String.concat "\n" (ExtString.String.nsplit s "\r\n")) "\r") in
	node "haxe_doc" [] [gen_string s]

let gen_doc_opt d =
	match d with
	| None -> []
	| Some s -> [gen_doc s]

let gen_arg_name (name,opt,_) =
	(if opt then "?" else "") ^ name

let real_path path meta =
	let rec loop = function
		| [] -> path
		| (Meta.RealPath,[(Ast.EConst (Ast.String s),_)],_) :: _ -> parse_path s
		| _ :: l -> loop l
	in
	loop meta

let tpath t =
	let i = t_infos t in
	real_path i.mt_path i.mt_meta

let rec follow_param t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow_param t
		| _ -> t)
	| TType ({ t_path = [],"Null" } as t,tl) ->
		follow_param (apply_params t.t_types tl t.t_type)
	| _ ->
		t

let gen_meta meta =
	let meta = List.filter (fun (m,_,_) -> match m with Meta.Used | Meta.MaybeUsed | Meta.RealPath -> false | _ -> true) meta in
	match meta with
	| [] -> []
	| _ ->
		let nodes = List.map (fun (m,el,_) ->
			node "m" ["n",fst (MetaInfo.to_string m)] (List.map (fun e -> node "e" [] [gen_string (Ast.s_expr e)]) el)
		) meta in
		[node "meta" [] nodes]

let rec gen_type t =
	match t with
	| TMono m -> (match !m with None -> tag "unknown" | Some t -> gen_type t)
	| TEnum (e,params) -> gen_type_decl "e" (TEnumDecl e) params
	| TInst (c,params) -> gen_type_decl "c" (TClassDecl c) params
	| TAbstract (a,params) -> gen_type_decl "x" (TAbstractDecl a) params
	| TType (t,params) -> gen_type_decl "t" (TTypeDecl t) params
	| TFun (args,r) -> node "f" ["a",String.concat ":" (List.map gen_arg_name args)] (List.map gen_type (List.map (fun (_,opt,t) -> if opt then follow_param t else t) args @ [r]))
	| TAnon a -> node "a" [] (pmap (fun f -> gen_field [] { f with cf_public = false }) a.a_fields)
	| TDynamic t2 -> node "d" [] (if t == t2 then [] else [gen_type t2])
	| TLazy f -> gen_type (!f())

and gen_type_decl n t pl =
	let i = t_infos t in
	node n [gen_path (tpath t) i.mt_private] (List.map gen_type pl)

and gen_field att f =
	let add_get_set acc name att =
		match acc with
		| AccNormal | AccResolve | AccRequire _ -> att
		| AccNo | AccNever -> (name, "null") :: att
		| AccCall -> (name,"accessor") :: att
		| AccInline -> (name,"inline") :: att
	in
	let att = (match f.cf_expr with None -> att | Some e -> ("line",string_of_int (Lexer.get_error_line e.epos)) :: att) in
	let att = (match f.cf_kind with
		| Var v -> add_get_set v.v_read "get" (add_get_set v.v_write "set" att)
		| Method m ->
			(match m with
			| MethNormal | MethMacro -> ("set", "method") :: att
			| MethDynamic -> ("set", "dynamic") :: att
			| MethInline -> ("get", "inline") :: ("set","null") :: att)
	) in
	let att = (match f.cf_params with [] -> att | l -> ("params", String.concat ":" (List.map (fun (n,_) -> n) l)) :: att) in
	let overloads = match List.map (gen_field []) f.cf_overloads with
		| [] -> []
		| nl -> [node "overloads" [] nl]
	in
	node f.cf_name (if f.cf_public then ("public","1") :: att else att) (gen_type f.cf_type :: gen_meta f.cf_meta @ gen_doc_opt f.cf_doc @ overloads)

let gen_constr e =
	let doc = gen_doc_opt e.ef_doc in
	let args, t = (match follow e.ef_type with
		| TFun (args,_) ->
			["a",String.concat ":" (List.map gen_arg_name args)] ,
			List.map (fun (_,opt,t) -> gen_type (if opt then follow_param t else t)) args @ doc
		| _ ->
			[] , doc
	) in
	node e.ef_name args t

let gen_ordered_constr e =
	let rec loop el = match el with
		| n :: el ->
			gen_constr (PMap.find n e.e_constrs) :: loop el
		| [] ->
			[]
	in
	loop e.e_names

let gen_type_params ipos priv path params pos m =
	let mpriv = (if priv then [("private","1")] else []) in
	let mpath = (if m.m_path <> path then [("module",snd (gen_path m.m_path false))] else []) in
	let file = (if ipos && pos <> null_pos then [("file",pos.pfile)] else []) in
	gen_path path priv :: ("params", String.concat ":" (List.map fst params)) :: (file @ mpriv @ mpath)

let gen_class_path name (c,pl) =
	node name [("path",s_type_path (tpath (TClassDecl c)))] (List.map gen_type pl)

let rec exists f c =
	PMap.exists f.cf_name c.cl_fields ||
			match c.cl_super with
			| None -> false
			| Some (csup,_) -> exists f csup

let rec gen_type_decl com pos t =
	let m = (t_infos t).mt_module in
	match t with
	| TClassDecl c ->
		let stats = List.map (gen_field ["static","1"]) (List.filter (fun cf -> cf.cf_name <> "__meta__") c.cl_ordered_statics) in
		let fields = (match c.cl_super with
			| None -> List.map (fun f -> f,[]) c.cl_ordered_fields
			| Some (csup,_) -> List.map (fun f -> if exists f csup then (f,["override","1"]) else (f,[])) c.cl_ordered_fields
		) in
		let fields = List.map (fun (f,att) -> gen_field att f) fields in
		let constr = (match c.cl_constructor with None -> [] | Some f -> [gen_field [] f]) in
		let impl = List.map (gen_class_path (if c.cl_interface then "extends" else "implements")) c.cl_implements in
		let tree = (match c.cl_super with
			| None -> impl
			| Some x -> gen_class_path "extends" x :: impl
		) in
		let doc = gen_doc_opt c.cl_doc in
		let meta = gen_meta c.cl_meta in
		let ext = (if c.cl_extern then [("extern","1")] else []) in
		let interf = (if c.cl_interface then [("interface","1")] else []) in
		let dynamic = (match c.cl_dynamic with
			| None -> []
			| Some t -> [node "haxe_dynamic" [] [gen_type t]]
		) in
		node "class" (gen_type_params pos c.cl_private (tpath t) c.cl_types c.cl_pos m @ ext @ interf) (tree @ stats @ fields @ constr @ doc @ meta @ dynamic)
	| TEnumDecl e ->
		let doc = gen_doc_opt e.e_doc in
		let meta = gen_meta e.e_meta in
		node "enum" (gen_type_params pos e.e_private (tpath t) e.e_types e.e_pos m) (gen_ordered_constr e  @ doc @ meta)
	| TTypeDecl t ->
		let doc = gen_doc_opt t.t_doc in
		let meta = gen_meta t.t_meta in
		let tt = gen_type t.t_type in
		node "typedef" (gen_type_params pos t.t_private t.t_path t.t_types t.t_pos m) (tt :: doc @ meta)
	| TAbstractDecl a ->
		let doc = gen_doc_opt a.a_doc in
		let meta = gen_meta a.a_meta in
		let mk_cast (t,cfo) = node "icast" (match cfo with None -> [] | Some cf -> ["field",cf.cf_name]) [gen_type t] in
		let sub = (match a.a_from with [] -> [] | l -> [node "from" [] (List.map mk_cast l)]) in
		let super = (match a.a_to with [] -> [] | l -> [node "to" [] (List.map mk_cast l)]) in
		let impl = (match a.a_impl with None -> [] | Some c -> [node "impl" [] [gen_type_decl com pos (TClassDecl c)]]) in
		let this = [node "this" [] [gen_type a.a_this]] in
		node "abstract" (gen_type_params pos a.a_private (tpath t) a.a_types a.a_pos m) (sub @ this @ super @ doc @ meta @ impl)

let att_str att =
	String.concat "" (List.map (fun (a,v) -> Printf.sprintf " %s=\"%s\"" a v) att)

let rec write_xml ch tabs x =
	match x with
	| Node (name,att,[]) ->
		IO.printf ch "%s<%s%s/>" tabs name (att_str att)
	| Node (name,att,[x]) ->
		IO.printf ch "%s<%s%s>" tabs name (att_str att);
		write_xml ch "" x;
		IO.printf ch "</%s>" name;
	| Node (name,att,childs) ->
		IO.printf ch "%s<%s%s>\n" tabs name (att_str att);
		List.iter (fun x ->
			write_xml ch (tabs ^ "\t") x;
			IO.printf ch "\n";
		) childs;
		IO.printf ch "%s</%s>" tabs name
	| PCData s ->
		IO.printf ch "%s" s
	| CData s ->
		IO.printf ch "<![CDATA[%s]]>" s

let generate com file =
	let t = Common.timer "construct xml" in
	let x = node "haxe" [] (List.map (gen_type_decl com true) (List.filter (fun t -> not (Meta.has Meta.NoDoc (t_infos t).mt_meta)) com.types)) in
	t();
	let t = Common.timer "write xml" in
	let ch = IO.output_channel (open_out_bin file) in
	write_xml ch "" x;
	IO.close_out ch;
	t()

let gen_type_string ctx t =
	let x = gen_type_decl ctx false t in
	let ch = IO.output_string() in
	write_xml ch "" x;
	IO.close_out ch


(* -------------------------------------------------------------------------- *)
(* PRINT HX FROM TYPE *)

let rec create_dir acc = function
	| [] -> ()
	| d :: l ->
		let path = acc ^ "/" ^ d in
		(try Unix.mkdir path 0o777 with _ -> ());
		create_dir path l

let conv_path p =
	match List.rev (fst p) with
	| x :: l when x.[0] = '_' -> List.rev (("priv" ^ x) :: l), snd p
	| _ -> p

let generate_type com t =
	let base_path = "hxclasses" in
	let pack , name = conv_path (t_path t) in
	create_dir "." (base_path :: pack);
	match pack, name with
	| ["flash";"net"], "NetStreamPlayTransitions"
	| ["flash";"filters"], "BitmapFilterQuality"
	| ["flash";"display"], ("BitmapDataChannel" | "GraphicsPathCommand")  -> ()
	| _ ->
	let f = open_out_bin (base_path ^ "/" ^ (match pack with [] -> "" | l -> String.concat "/" l ^ "/") ^ name ^ ".hx") in
	let ch = IO.output_channel f in
	let p fmt = IO.printf ch fmt in
	if pack <> [] then IO.printf ch "package %s;\n\n" (String.concat "." pack);
	let rec notnull t =
		match t with
		| TMono r ->
			(match !r with
			| None -> t
			| Some t -> notnull t)
		| TLazy f ->
			notnull ((!f)())
		| TType ({ t_path = [],"Null" },[t]) ->
			t
		| _ ->
			t
	in
	let rec path p tl =
		let p = conv_path p in
		(if fst p = pack then snd p else s_type_path p) ^ (match tl with [] -> "" | _ -> "<" ^ String.concat "," (List.map stype tl) ^ ">")
	and stype t =
		match t with
		| TMono r ->
			(match !r with
			| None -> "Unknown"
			| Some t -> stype t)
		| TInst ({ cl_kind = KTypeParameter _ } as c,tl) ->
			path ([],snd c.cl_path) tl
		| TInst (c,tl) ->
			path c.cl_path tl
		| TEnum (e,tl) ->
			path e.e_path tl
		| TType (t,tl) ->
			path t.t_path tl
		| TAbstract (a,tl) ->
			path a.a_path tl
		| TAnon a ->
			let fields = PMap.fold (fun f acc -> (f.cf_name ^ " : " ^ stype f.cf_type) :: acc) a.a_fields [] in
			"{" ^ String.concat ", " fields ^ "}"
		| TLazy f ->
			stype ((!f)())
		| TDynamic t2 ->
			if t == t2 then "Dynamic" else "Dynamic<" ^ stype t2 ^ ">"
		| TFun ([],ret) ->
			"Void -> " ^ ftype ret
		| TFun (args,ret) ->
			String.concat " -> " (List.map (fun (_,_,t) -> ftype t) args) ^ " -> " ^ ftype ret
	and ftype t =
		match t with
		| TMono r ->
			(match !r with
			| None -> stype t
			| Some t -> ftype t)
		| TLazy f ->
			ftype ((!f)())
		| TFun _ ->
			"(" ^ stype t ^ ")"
		| _ ->
			stype t
	in
	let sparam (n,v,t) =
		match v with
		| None ->
			n ^ " : " ^ stype t
		| Some (Ident "null") ->
			"?" ^ n ^ " : " ^ stype (notnull t)
		| Some v ->
			n ^ " : " ^ stype t ^ " = " ^ (match s_constant v with "nan" -> "0./*NaN*/" | v -> v)
	in
	let print_meta ml =
		List.iter (fun (m,pl,_) ->
			match m with
			| Meta.DefParam | Meta.CoreApi | Meta.Used | Meta.MaybeUsed -> ()
			| _ ->
			match pl with
			| [] -> p "@%s " (fst (MetaInfo.to_string m))
			| l -> p "@%s(%s) " (fst (MetaInfo.to_string m)) (String.concat "," (List.map Ast.s_expr pl))
		) ml
	in
	let access a =
		match a, pack with
		| AccNever, "flash" :: _ -> "null"
		| _ -> s_access a
	in
	let print_field stat f =
		p "\t";
		print_meta f.cf_meta;
		if stat then p "static ";
		(match f.cf_kind with
		| Var v ->
			p "var %s" f.cf_name;
			if v.v_read <> AccNormal || v.v_write <> AccNormal then p "(%s,%s)" (access v.v_read) (access v.v_write);
			p " : %s" (stype f.cf_type);
		| Method m ->
			let params, ret = (match follow f.cf_type with
				| TFun (args,ret) ->
					List.map (fun (a,o,t) ->
						let rec loop = function
							| [] -> Ident "null"
							| (Meta.DefParam,[(EConst (String p),_);(EConst v,_)],_) :: _ when p = a ->
								(match v with
								| Float "1.#QNAN" -> Float "0./*NaN*/"
								| Float "4294967295." -> Int "0xFFFFFFFF"
								| Int "16777215" -> Int "0xFFFFFF"
								| Float x ->
									(try
										let f = float_of_string x in
										let s = string_of_int (int_of_float f) in
										if s ^ "." = x then Int s else v
									with _ ->
										v)
								| _ -> v)
							| _ :: l -> loop l
						in
						a,(if o then Some (loop f.cf_meta) else None ),t
					) args, ret
				| _ ->
					assert false
			) in
			let tparams = (match f.cf_params with [] -> "" | l -> "<" ^ String.concat "," (List.map fst l) ^ ">") in
			p "function %s%s(%s) : %s" f.cf_name tparams (String.concat ", " (List.map sparam params)) (stype ret);
		);
		p ";\n"
	in
	(match t with
	| TClassDecl c ->
		print_meta c.cl_meta;
		p "extern %s %s" (if c.cl_interface then "interface" else "class") (stype (TInst (c,List.map snd c.cl_types)));
		let ext = (match c.cl_super with
		| None -> []
		| Some (c,pl) -> [" extends " ^ stype (TInst (c,pl))]
		) in
		let ext = List.fold_left (fun acc (i,pl) -> ((if c.cl_interface then " extends " else " implements ") ^ stype (TInst (i,pl))) :: acc) ext c.cl_implements in
		let ext = (match c.cl_dynamic with
			| None -> ext
			| Some t ->
				(match c.cl_path with
				| ["flash";"errors"], _ -> ext
				| _ when t == t_dynamic -> " implements Dynamic" :: ext
				| _ -> (" implements Dynamic<" ^ stype t ^ ">") :: ext)
		) in
		let ext = (match c.cl_path with
			| ["flash";"utils"], "ByteArray" -> " implements ArrayAccess<Int>" :: ext
			| ["flash";"utils"], "Dictionnary" -> [" implements ArrayAccess<Dynamic>"]
			| ["flash";"xml"], "XML" -> [" implements Dynamic<XMLList>"]
			| ["flash";"xml"], "XMLList" -> [" implements ArrayAccess<XML>"]
			| ["flash";"display"],"MovieClip" -> [" extends Sprite #if !flash_strict implements Dynamic #end"]
			| ["flash";"errors"], "Error" -> [" #if !flash_strict implements Dynamic #end"]
			| _ -> ext
		) in
		p "%s" (String.concat "" (List.rev ext));
		p " {\n";
		let sort l =
			let a = Array.of_list (List.filter (fun f -> f.cf_public && not (List.memq f c.cl_overrides)) l) in
			let name = function "new" -> "" | n -> n in
			Array.sort (fun f1 f2 ->
				match f1.cf_kind, f2.cf_kind with
				| Var _, Var _ | Method _ , Method _ -> compare (name f1.cf_name) (name f2.cf_name)
				| Var _, _ -> -1
				| _ -> 1
			) a;
			Array.to_list a
		in
		List.iter (print_field false) (sort (match c.cl_constructor with None -> c.cl_ordered_fields | Some f -> f :: c.cl_ordered_fields));
		List.iter (print_field true) (sort c.cl_ordered_statics);
		p "}\n";
	| TEnumDecl e ->
		print_meta e.e_meta;
		p "extern enum %s {\n" (stype (TEnum(e,List.map snd e.e_types)));
		let sort l =
			let a = Array.of_list l in
			Array.sort compare a;
			Array.to_list a
		in
		List.iter (fun n ->
			let c = PMap.find n e.e_constrs in
			p "\t%s" c.ef_name;
			(match follow c.ef_type with
			| TFun (args,_) -> p "(%s)" (String.concat ", " (List.map sparam (List.map (fun (a,o,t) -> a,(if o then Some (Ident "null") else None),t) args)))
			| _ -> ());
			p ";\n";
		) (if Meta.has Meta.FakeEnum e.e_meta then sort e.e_names else e.e_names);
		p "}\n"
	| TTypeDecl t ->
		print_meta t.t_meta;
		p "typedef %s = " (stype (TType (t,List.map snd t.t_types)));
		p "%s" (stype t.t_type);
		p "\n";
	| TAbstractDecl a ->
		print_meta a.a_meta;
		p "abstract %s {}" (stype (TAbstract (a,List.map snd a.a_types)));
	);
	IO.close_out ch

let generate_hx com =
	List.iter (generate_type com) com.types

