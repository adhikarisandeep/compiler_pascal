{-----Sandeep Adhikari-----}
{-----all my modifications start with ----- and end with -----}

Program Compiler;
const
     norw = 28;         { no. of reserved words }				{-----number of reserved words changed-----}
     txmax = 100;       { length of identifier table }
     nmax = 14;         { max. no of digits in numbers }
     al = 10;           { length of identifiers }
     amax= 2047;        { maximum address }
     levmax = 5;        { maximum depth of block nesting }
     cxmax = 500;       { size of code array }
     stacksize = 255;   { size of stack }
     p_max = 100;       { maximum no of parameters }
type
    symbol = (nul,ident,number,plus,minus,times,slash,oddsym,
     eql,neq,lss,leq,gtr,geq,lparen,rparen,comma,semicolon,colon,
     period,becomes,beginsym,elsesym,endsym,ifsym,thensym,
     whilesym,dosym,callsym,constsym,varsym,repeatsym,untilsym,procsym,
     forsym,tosym,downtosym,casesym,ofsym,cendsym,writelnsym,writesym,funcsym,andsym,orsym,notsym,valsym,refsym); {-----val/ref sym added-----}
    alfa = packed array[1..al] of char;
    objects = (constant,variable,procdure,funct,ref,valu); 			 {-----added ref/val type-----}
    fct=(LIT,OPR,LOD,STO,CAL,INT,JMP,JPC,CTS,STI,LDI,LDA); (* Functions *)	 {-----added new commands STI,LDI,LDA-----}
    instruction= packed record
                        f:fct;         (* function code *)
                        lvl:0..levmax; (* level *)
                        ad:-levmax..amax;    (* displacement address *)
                       end;
var
   ch: char;            { last character read }
   sym:symbol;          { last symbol read }

   id :alfa;            { last identifier read }
   num: integer;      { last number read }
   charcnt : integer;   { character count }
   linlen : integer;    { line length }
   kk : integer;
   codeinx: integer;    { code allocation index }
   line: array [1..81] of char;
   a:alfa;
   code: array [0..cxmax] of instruction;
   word: array [1..norw] of alfa;
   wsym: array [1..norw] of symbol;
   ssym: array [char] of symbol;
   mnemonic: array[fct] of packed array[1..3] of char;
   table: array [0..txmax] of record
            name: alfa;
	    params: array [0..txmax] of integer;				{-----added params-----}
             case kind: objects of
            constant:(val : integer);
            variable,procdure,funct,valu,ref:(level,adr:integer);{-----added value and reference-----}
            end;

procedure error(I:integer);
begin
   case I of
     1:writeln(' Use =  instead of :=');
     2:writeln(' = must be followed by a number.');
     3:writeln(' Identifier must be followed by =.');
     4:writeln(' Const, Var, Procedure must be followed by an identifier.');
     5:writeln(' Semicolon or comma missing.');
     6:writeln(' Incorreet symbol after procedue declaration.');
     7:writeln(' Statement expected.');
     8:writeln(' Incorrect symbol after statement part in block.');
     9:writeln(' Period expected.');
     10:writeln(' Semicolon between statements is missing.');
     11:writeln(' Undeclared identifier.');
     12:writeln(' Assignment to constant  or  procedure is not allowed.');
     13:writeln(' Assignment operator := expected.');
     14:writeln(' call must be followed by an identifier.');
     15:writeln(' Call of a constant or a variable is meaningless.');
     16:writeln(' Then expected.');
     17:writeln(' Semicolon or end expected.');
     18:writeln(' DO expected.');
     19:writeln(' Incorrect symbol following statement.');
     20:writeln(' Relational operator expected.');
     21:writeln(' Expression must not contain a procedure identifier.');
     22:writeln(' Right parenthesis missing.');
     23:writeln(' The preceding factor cannot be followed by this symbol.');
     24:writeln(' An expression cannot begin with this symbol.');
     25:writeln(' UNTIL expected.');
     26:writeln(' TO or DOWNTO is expected.');
     27:writeln(' OF expected.');
     28:writeln(' : expected.');
     29:writeln(' Constant or Number is expected.');
     30:writeln(' This number is too large.');
     31:writeln(' CEND is expected.');
     32:writeln(' FOR must be followd by identifier. ');
     33:writeln(' Duplicate identifier.');
     34:writeln(' Procedures nested too deep.');
     35:writeln(' Program Too Long.');
     
     36:writeln(' Left parenthesis missing.');
     37:writeln(' Variable expected.');
     38:writeln(' OF expected.');
     39:writeln(' Constant expected.');
     40:writeln(' Assignment to function is not allowed.');
     41:writeln(' Call must be followed by a function.');
     42:writeln(' Variable or function expected.');
     43:writeln(' Cannot assign value to this function.');
			{-----added error messaged-----}
     44:writeln(' VAL or REF expected.');
     45:writeln(' Ident expected.');
     46:writeln(' VAL,REF, or variable or function expected.');
     47:writeln(' Cannot pass number or constant to a REF parameter.');
			{-----finished adding error messages-----}
   end;
   HALT
end;

procedure gen(x:fct; y,z:integer);
begin
     if codeinx >cxmax then error(35);
     with code[codeinx] do
      begin
       f:=x;
       lvl:=y;
       ad:=z
      end;
     codeinx:=codeinx+1
end;

procedure getsym; {this procedure classifies each token as a particular symbol. }
 var i,j,k:integer;

 procedure getch;
  {this procedure reads in a line at a time from the input file and stores
   it in an array. A character at atime is read from the array }
  begin {getch}
   if charcnt=linlen then
    begin
      if eof then
       begin
        writeln('PROGRAM INCOMPLETE');
        if (sym=endsym) then error(9);
       end;
      linlen:=0; charcnt:=0;
      while not eoln do
       begin
         linlen:=linlen+1;
         read(ch);
         write(ch);
         ch:=upcase(ch);
         line[linlen]:=ch {read in a line from input file }
       end;
      writeln();
      linlen:=linlen+1;
      readln();
      line[linlen]:=' ' { initialize last character or string as a spaces }
    end;
   charcnt:=charcnt+1;
   ch:=line[charcnt]
  end; {getch}

begin { getsym}
 while ch=' ' do getch;
 if ch in ['A'..'Z'] then
  begin { identifier or keyword}
   k:=0;
   repeat
    if k< al then
     begin
      k:=k+1;
      a[k]:=ch;
     end;
    getch
   until not (ch in ['A'..'Z','0'..'9']);
   if k >= kk then kk:=k
   else
    repeat
     a[kk]:=' ';
     kk:=kk-1
    until kk=k;
   id:=a;               {binary search to determine if id is a reseved word}
   i:=1;
   j:=norw;
   repeat
      k:=(i+j) div 2;
      if id <=word[k] then j:=k-1 ;
      if id >= word[k] then i:=k+1;
   until  i>j;
   if i-1>j then  sym:=wsym[k] { reserved word or identifier }
   else sym:=ident;
  end
 else
  if ch in ['0'..'9'] then
   begin
    k:=0;
    num:=0;
    sym:=number;
    repeat
     num:=10*num+(ord(ch)-ord('0'));
     k:=k+1;
     getch
    until not (ch in ['0'..'9']);
    if k>nmax then error(30)
   end
  else
   if ch=':' then
    begin
     getch;
     if ch='=' then
      begin
       sym:=becomes;
       getch
      end
     else
      sym:=colon
    end
    else
     if ch='>' then
      begin
       getch;
       if ch='=' then
        begin
         sym:=geq;
         getch
        end
       else sym:=gtr
      end
     else
      if ch='<' then
       begin
        getch;
        if ch='=' then
         begin
          sym:=leq;
          getch
         end
        else
         if ch='>' then
          begin
           sym:=neq;
           getch
          end
         else
          sym:=lss
       end
      else
       begin
        sym:=ssym[ch];
        getch
       end
end; {getsym}

procedure block(lev,tx:integer);
var
   dx,                  (* Data allocation index *)
   tabinx0,             (* Initial table index   *)
   codeinx0:integer;    (* Initial code index   *)
   symType:symbol;	{-----to save the sym for proc or funct-----}

function position(k:alfa):integer;
var
  i:integer;
begin
  table[0].name:=k;
  i:=tx;
  while table[i].name<>k do
   i:=i-1;
  position:=i;
end;

procedure enter(k:objects);
begin
         tx:=tx+1;
         with table[tx] do
          begin
            name:=id;
            kind:=k;
            case k of
                 constant: begin
                            if num>amax then error(30);
                            val:=num
                            end;
                 variable,valu,ref:begin		{-----added value,and reference also-----}
                           level:=lev;
	                   adr:=dx;
                           dx:=dx+1;
                          end;
                 procdure: level:=lev;
		 funct: level:=lev;
            end
          end

end;

procedure listcode;
var i:integer;
begin (*list code generated for this block *)
      writeln;
      for i:=codeinx0 to codeinx - 1 do
       with code[i] do
        writeln(i,' ',mnemonic[f],' ',lvl,' ',ad)
end;

procedure constdeclaration;
begin
 if sym=ident then
  begin
   getsym;
   if sym=eql then
    begin
     getsym;
     if sym=number then
      begin
       enter(constant);
       getsym;
      end
     else error(2)
    end
   else error(3)
  end
 else error(4)
end;

procedure vardeclaration;
begin
 if sym=ident then
  begin
       enter(variable);
       getsym;
  end
 else error(4)
end;


procedure statement;
var
    cx1,cx2:integer;
	{-----added new variable-----}
    symSave:symbol;
    saveKind:objects;
    first,value,i,p,saveID:integer;
	{-----finished adding variable-----}
 
procedure generalExpression;	{-----condition renamed to general expression-----}
  var
  relop:symbol;
    
procedure expression;
    var plusop:symbol;
 
 procedure term;
    var multop:symbol;

  procedure factor;
  var i : integer;
    
  begin { factor }
    if sym = ident then
     begin
       i := position(id);
       if i = 0  then error(11)
       else
        with table[i] do
         case kind of
          variable,valu:gen(LOD,lev-level,adr);		{-----added val and ref support-----}
          constant:gen(LIT,0,val);
	  ref:gen(LDI,lev-level,adr);
          procdure:error(12);
	  funct:error(40)	{-----function not allowed-----}
        end;
       getsym
     end
    else if sym = number then
      begin
        gen(LIT,0,num);
        getsym
      end
     else if sym = lparen then
       begin
        getsym;
        generalExpression;
         if sym = rparen then
         getsym
        else error(22)
       end
     else if (sym = callsym) then
	     begin
		getsym();
		if sym<>ident then error(14);
		i:=position(id);
		if i=0 then error(11);
		if (table[i].kind<>funct) then error(41);
		gen(INT,0,1);
		saveID:=i;
		getsym();
		if (sym=lparen) then
		begin
			p:=0;
			gen(INT,0,3);
			repeat
				if (table[saveID].params[p]=1) then 
				begin
					getsym;
					if sym<> ident then error(14)
					else 
					begin
						i:=position(id);
						if i=0 then error(11) else
							if ((sym=number) or (table[i].kind=constant)) then error(47);
						if (table[i].kind=variable) or (table[i].kind=valu) then 
							with table[i] do
								gen(LDA,lev-level,adr)
						else if (table[i].kind=ref) then 
							with table[i] do
								gen(LOD,lev-level,adr);
						getsym;
					end;
				end
				else if (table[saveID].params[p]=0) then
				begin
					getsym;
					generalExpression;
				end;
				p := p+1;
			until (sym<>comma);
			if (sym=rparen) then getsym else error(22);
			gen(INT,0,-(3+p))
		end;
		with table[saveID] do
		    gen(CAL,lev-level,adr);
	     end
     else if sym=notsym then
	     begin
		getsym();
		factor;
		gen(LIT,0,0);
		gen(OPR,0,8);
	     end
      else error(24)
  end; { factor }

 begin { term }
  factor;
  while sym in [times,slash,andsym] do
   begin
    multop:=sym;
    getsym;
    factor;
    if ((multop=times) or (multop=andsym)) then gen(OPR,0,4)
    else gen(OPR,0,5)
   end;
 end; { term }

begin { expression }
 if sym in [plus,minus] then
  begin
   plusop:=sym;
   getsym;
   term;
   if plusop=minus then gen(OPR,0,1)
  end
 else
  term;
  while sym in [plus,minus,orsym] do	{-----added or to work with expression-----}
   begin
    plusop:=sym;
    getsym;
    term;
    if plusop=minus then  gen(OPR,0,3)
    else gen(OPR,0,2)
   end;
end; { expression }
    
begin {-----generalExpression-----}
    if sym = oddsym then
      begin
        getsym;
        expression;
        gen(OPR,0,6)
      end
    else
     begin
        expression;
        if (sym in [eql,neq,lss,leq,gtr,geq]) then
         begin
           relop:=sym;
           getsym;
           expression;
           case relop of
            eql:gen(OPR,0,8);
            neq:gen(OPR,0,9);
            lss:gen(OPR,0,10);
            geq:gen(OPR,0,11);
            gtr:gen(OPR,0,12);
            leq:gen(OPR,0,13)
           end;
         end
     end
 end; {----- generalExpression -----}
    
begin { statement }
 case sym of
 ident:begin
            i:=position(id);
            if i=0 then error(11)
            else
             if ((table[i].kind<>variable) and (table[i].kind<>funct) and (table[i].kind<>ref) and (table[i].kind<>valu)) then error(46); {-----supports val, and ref now-----}
		saveKind := table[i].kind;
		getsym;
            if sym=becomes then getsym else error(13);
            generalExpression;
						{-----additions after this-----}
	    if ((saveKind = variable) or (saveKind = valu)) then 
		    with table[i] do
			    gen(STO,lev-level,adr)
	    else if ((saveKind = funct)  and (i= tabinx0)) then gen(STO,0,-1)
	    else if (saveKind = ref) then 
		    with table[i] do
			    gen(STI,lev-level,adr)
	    else if i<>tabinx0 then error(43)
						{-----finished adding new stuff-----}
         end;
			{-----new changes to callsym-----}
 callsym:begin
            getsym;
            if sym<>ident then error(14);
                  i:=position(id);
	          saveID := i;
                  if i=0 then error(11);
		  if table[i].kind<>procdure then error(15);
                  getsym;

		if (sym=lparen) then
		begin
			p:=0;
			gen(INT,0,3);
			repeat
				if (table[saveID].params[p]=1) then 
				begin
					getsym;
					if sym<> ident then error(14)
					else 
					begin
						i:=position(id);
						if i=0 then error(11) else
							if ((sym=number) and (table[i].kind=constant)) then error(47);
						if (table[i].kind=variable) or (table[i].kind=valu) then 
							with table[i] do
								gen(LDA,lev-level,adr)
						else if (table[i].kind=ref) then 
							with table[i] do
								gen(LOD,lev-level,adr);
						getsym;
					end;
				end
				else if (table[saveID].params[p]=0) then
				begin
					getsym;
					generalExpression;
				end;
				p := p+1;
			until (sym<>comma);
			if (sym=rparen) then getsym else error(22);
			gen(INT,0,-(3+p));
		end;
			with table[saveID] do
		                 gen(CAL,lev-level,adr)
		
         end;
			{----finished adding changes-----}
 ifsym:begin
            getsym;
            generalExpression;
            cx1:=codeinx;
            gen(JPC,0,0);
            if sym=thensym then getsym else error(16);
            statement;
            code[cx1].ad:=codeinx;
            (* Place Your Code for ELSE Here *)
	    if sym=elsesym then 
		    begin
			getsym;
			cx2:=codeinx;
			gen(JMP,0,0);
			code[cx1].ad:=codeinx;
			statement;
			code[cx2].ad:=codeinx
	    	    end
       end;
 beginsym:begin
            repeat
             getsym;
             statement
            until sym<>semicolon;
            if sym=endsym then getsym
	    else 
		    Begin
			error(17)
		    end
          end;
 whilesym:begin
            getsym;
            cx1:=codeinx;
            generalExpression;
            cx2:=codeinx;
            gen(JPC,0,0);
            if sym=dosym then getsym
            else error(18);
            statement;
            gen(JMP,0,cx1);
            code[cx2].ad:=codeinx
          end;
 repeatsym:begin
                (* Place Your Code for REPEAT UNTIL Here *)
	     getsym;
       cx1:=codeinx;
             statement;
             while sym=semicolon do
               begin
                 getsym;
                 statement
               end;
             if sym=untilsym then getsym else error(25);
             generalExpression;
	     gen(JPC,0,cx1)
           end;
 forsym:begin
                (* Place Your Code for FOR Here *)
	    getsym;
          if sym<>ident then error(32)
          else
            begin
              i:=position(id);
              if i=0 then error(11)
              else
              if table[i].kind<>variable then error(37);
              getsym
            end;
          if sym=becomes then getsym else error(13);
          generalExpression;
	  symSave := sym;
	  with table[i] do
               gen(STO,lev-level,adr);
	    if sym=tosym then getsym else
		if sym=downtosym then getsym else error(26);
	  generalExpression;
	  cx1:=codeinx;
	  gen(CTS,0,0);
	  with table[i] do
		  gen(LOD,lev-level,adr);
	  if symSave = tosym then gen(OPR,0,11);
	  if symSave = downtosym then gen(OPR,0,13);
	  cx2:=codeinx;
	  gen(JPC,0,0);
          if sym=dosym then getsym else error(18);
          statement;
	  with table[i] do
		  gen(LOD,lev-level,adr);
	  gen(LIT,0,1);
	  if symSave=tosym then gen(OPR,0,2)
		  else if symSave=downtosym then gen(OPR,0,3);
	  with table[i] do
		  gen(STO,lev-level,adr);
	  gen(JMP,0,cx1);
	  code[cx2].ad:=codeinx;
	  gen(INT,0,-1)
        end;
 casesym:begin
                (* Place Your Code for CASE Here *)
		first := 1;
		getsym;
		generalExpression;
		if sym=ofsym then getsym else error(38);
		while (sym=ident) or (sym=number) do
		begin
			if sym=ident then
			begin
				i:=position(id);
				if i=0 then error(11)
				else
				if table[i].kind<>constant then error(39);
				with table[i] do
					value := val;
			end;
			gen(CTS,0,0);
			if sym=number then gen(LIT,0,num)
			else if sym = ident then gen(LIT,0,value);
			gen(OPR,0,8);
			cx1:=codeinx;
			gen(JPC,0,0);
			getsym;             
			if sym=colon then getsym else error(34);
			statement;
			if first=1 then
				begin
					cx2:=codeinx;
					gen(JMP,0,0);
					first :=0
				end
			else
				begin
					gen(JMP,0,cx2)
				end;
        code[cx1].ad:=codeinx;
			if sym=semicolon then getsym else error(35)
		end;
		if sym=cendsym then getsym else error(37);
		if first <>1 then code[cx2].ad:=codeinx;
			gen(INT,0,-1);
         end;
 writelnsym:begin
                 (* Place Your Code for WRITELN Here *)
	    getsym;
            if sym=lparen then getsym else error(36);
            generalExpression;
	    gen(OPR,0,14);
            while sym=comma do
            begin
              getsym;
              generalExpression;
	      gen(OPR,0,14)
            end;
            if sym=rparen then getsym else error(22);
	    gen(OPR,0,15)
          end;
 writesym:begin
                 (* Place Your Code for WRITE Here *)
		getsym;
		if sym=lparen then getsym else error(36);
		generalExpression;
		gen(OPR,0,14);
		while sym=comma do
		begin
			getsym;
			generalExpression;
			gen(OPR,0,14)
		end;
		if sym=rparen then getsym else error(22)
		end;
end { case }
end; { statement }

begin { block }
    dx:=3;
    tabinx0:=tx;
    table[tx].adr:=codeinx;
    { adr of JMP is saved in table under procedure name }
    gen(JMP,0,0);
    if lev > levmax then error(34);
			{-----added modifications-----}
	if (lev>0) then
	    begin
		if sym=lparen then
		begin
			repeat
				getsym;
				symtype:=sym;
				if ((sym<>valsym) and (sym<>refsym)) then error(44);
				repeat
					getsym;
					if sym=ident then getsym else error(45);
					if (symtype=valsym) then
						begin
							enter(valu);
							table[tabinx0].params[dx-4]:=0;
						end
					else if (symtype=refsym) then
						begin
							enter(ref);							
							table[tabinx0].params[dx-4]:=1;
						end
				until (sym<>comma);
			until (sym<>semicolon);
			if sym=rparen then getsym else error(22);
		end;
		if sym=semicolon then getsym else error(10);
	    end;
			{-----finished adding modifications-----}
    repeat
     if sym=constsym then
      begin
         getsym;
         constdeclaration;
         while sym=comma do
          begin
            getsym;
            constdeclaration
          end;
         if sym=semicolon then getsym
         else error(10);
      end;
      if sym=varsym then
      begin
         getsym;
         vardeclaration;
         while sym=comma do
          begin
            getsym;
            vardeclaration
          end;

         if sym=semicolon then getsym
         else error(10);
      end;
      while ((sym=procsym) or (sym=funcsym)) do
       begin
	   symType := sym;
           getsym;
           if sym=ident then
            begin
		getsym;
		if symType=procsym then enter(procdure) else enter(funct);
            end
           else error(4);
	   {-----semicolon removed to the top of the block-----}
           block(lev+1,tx);
           if sym=semicolon then getsym
		else error(10);
       end
    until not (sym  in [constsym,varsym,procsym,funcsym]);
    { modify JMP 0,0 to where is INT }
    code[table[tabinx0].adr].ad:=codeinx;
    { modify procedure adr to INT instead of JMP }
    table[tabinx0].adr:=codeinx;
    codeinx0:=codeinx;
    gen(INT,0,dx);
    statement;
    gen(OPR,0,0);
    listcode;
end; { block }

Procedure interpret;
var
   p,b,t:integer; (* program-, base-, topstack-registers *)
   i:instruction; (* instruction register *)
   s:array [1.. stacksize] of integer; (* datastore *)

function base(l:integer):integer;
var
   b1:integer;
begin
     b1:=b; (* find base l levels down *)
     while l > 0 do
     begin b1:=s[b1]; l:=l-1 end;
     base:=b1
end; (* base *)

begin (* interpret *)
      Writeln('Start PL/0');
      t:=0;
      b:=1;
      p:=0;
      s[1]:=0;
      s[2]:=0;
      s[3]:=0;
      repeat
            i:=code[p];
            p:=p+1;
            with i  do
                 case f of
                   LIT: begin
                             t:=t+1; s[t]:= ad
                        end;
                   OPR:
                       case ad of (* operator *)
                        0:begin (* return *)
                           t:=b-1;
                           p:=s[t+3];
                           b:=s[t+2]
                          end;
                        1: s[t]:=-s[t];
                        2: begin
                             t:=t-1;
                             s[t]:=s[t]+s[t+1]
                           end;
                        3:begin
                           t:=t-1;
                           s[t]:=s[t]-s[t+1]
                          end;
                        4:begin
                           t:=t-1;
                           s[t]:=s[t]*s[t+1]
                          end;
                        5:begin
                           t:=t-1;
                           s[t]:=s[t] div s[t+1]
                          end;
                        6:s[t]:=ord(odd(s[t]));
                        8:begin
                           t:=t-1;
                           s[t]:=ord(s[t]=s[t+1])
                          end;
                       9:begin
                           t:=t-1;
                           s[t]:=ord(s[t]<> s[t+1])
                          end;
                       10:begin
                           t:=t-1;
                           s[t]:=ord(s[t]<s[t+1])
                          end;
                       11:begin
                           t:=t-1;
                           s[t]:=ord(s[t]>=s[t+1])
                          end;
                       12:begin
                           t:=t-1;
                           s[t]:=ord(s[t]>s[t+1])
                          end;
                       13:begin
                           t:=t-1;
                           s[t]:=ord(s[t]<=s[t+1])
                          end;
                       14:begin
                           write(s[t],' ');
                           t:=t-1;
                          end;
                      15:writeln();

                   end;
                   LOD:begin
                           t:=t+1;
                           s[t]:=s[base(lvl)+ad]
                          end;
                   STO:begin
                           s[base(lvl)+ad]:=s[t];
                           t:=t-1
                        end;
                   CAL:begin     (* generate new block mark *)
                             s[t+1]:=base(lvl);
                             s[t+2]:=b;
                             s[t+3]:=p;
                             b:=t+1;
                             p:=ad
                        end;
                   INT: t:=t+ad;
                   JMP:p:=ad;
                   JPC:begin
                        if s[t]=lvl then p:=ad;
                        t:=t-1
                       end;
		   CTS:begin
			t:=t+1;
			s[t]:=s[t-1];
			end;
		   {-----added new commands-----}
		    STI:begin
			s[s[base(lvl)+ad]]:=s[t];
			t:=t-1;
			end;
		    LDI:begin
			t:=t+1;
			s[t]:=s[s[base(lvl)+ad]];
			end;
		    LDA:begin
			t:=t+1;
			s[t]:=base(lvl)+ad;
			end;
		   {-----end adding new commands-----}
		    
                 end
	until p=0;
      Writeln('End PL/0');
end; (* interpret *)

procedure Intialize;
begin
     for ch:=';' to 'A' do
       ssym[ch]:=nul;
     word[1]:='AND       ';
     word[2]:='BEGIN     ';
     word[3]:='CALL      ';
     word[4]:='CASE      ';
     word[5]:='CEND      ';
     word[6]:='CONST     ';
     word[7]:='DO        ';
     word[8]:='DOWNTO    ';
     word[9]:='ELSE      ';
     word[10]:='END       ';
     word[11]:='FOR       ';
     word[12]:='FUNCTION  ';
     word[13]:='IF        ';
     word[14]:='NOT       ';
     word[15]:='ODD       ';
     word[16]:='OF        ';
     word[17]:='OR        ';
     word[18]:='PROCEDURE ';
     word[19]:='REF       ';				{-----REF added-----}
     word[20]:='REPEAT    ';
     word[21]:='THEN      ';
     word[22]:='TO        ';
     word[23]:='UNTIL     ';
     word[24]:='VAL       ';				{-----VAL added-----}
     word[25]:='VAR       ';
     word[26]:='WHILE     ';
     word[27]:='WRITE     ';
     word[28]:='WRITELN   ';

     wsym[1]:=andsym;
     wsym[2]:=beginsym;
     wsym[3]:=callsym;
     wsym[4]:=casesym;
     wsym[5]:=cendsym;
     wsym[6]:=constsym;
     wsym[7]:=dosym;
     wsym[8]:=downtosym;
     wsym[9]:=elsesym;
     wsym[10]:=endsym;
     wsym[11]:=forsym;
     wsym[12]:=funcsym;
     wsym[13]:=ifsym;
     wsym[14]:=notsym;
     wsym[15]:=oddsym;
     wsym[16]:=ofsym;
     wsym[17]:=orsym;
     wsym[18]:=procsym;
     wsym[19]:=refsym;			{-----refsym added-----}
     wsym[20]:=repeatsym;
     wsym[21]:=thensym;
     wsym[22]:=tosym;
     wsym[23]:=untilsym;
     wsym[24]:=valsym;			{-----valsym added-----}
     wsym[25]:=varsym;
     wsym[26]:=whilesym;
     wsym[27]:=writesym;
     wsym[28]:=writelnsym;

     ssym['+']:=plus;
     ssym['-']:=minus;
     ssym['*']:=times;
     ssym['/']:=slash;
     ssym['(']:=lparen;
     ssym[')']:=rparen;
     ssym['=']:=eql;
     ssym[',']:=comma;
     ssym['.']:=period;
     ssym['#']:=neq;
     ssym['<']:=lss;
     ssym['>']:=gtr;
     ssym['"']:=leq;
     ssym['@']:=geq;
     ssym[';']:=semicolon;
     ssym[':']:=colon;

     mnemonic[LIT]:='LIT';
     mnemonic[OPR]:='OPR';
     mnemonic[LOD]:='LOD';
     mnemonic[STO]:='STO';
     mnemonic[CAL]:='CAL';
     mnemonic[INT]:='INT';
     mnemonic[JMP]:='JMP';
     mnemonic[JPC]:='JPC';
     mnemonic[CTS]:='CTS';
     {-----added new mnemonics-----}
     mnemonic[STI]:='STI';
     mnemonic[LDI]:='LDI';
     mnemonic[LDA]:='LDA';
     {-----finished adding-----}
     
     codeinx:=0;
     charcnt:=0;
     linlen:=0;
     ch:=' ';
     kk:=al;
     a:='          ';
     id:='          ';
end;

BEGIN { main program }
      Intialize;
      getsym;
      block(0,0);
      if (sym<>period) then error(9);
      writeln;
      writeln('Successful compilation!');
      writeln;
      interpret;
End. { main program }