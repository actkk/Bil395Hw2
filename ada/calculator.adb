with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Calculator is
   -- Değişkenleri tanımla
   Input : String(1..100);
   Last : Natural;
   
   -- Token türleri
   type Token_Type is (Number, Plus, Minus, Multiply, Divide, End_Of_Input);
   
   -- Token yapısı
   type Token is record
      Kind : Token_Type;
      Value : Integer;
   end record;
   
   -- Tokenizer için değişkenler
   Current_Position : Natural;
   Current_Token : Token;
   Expression : String(1..100);
   Expression_Length : Natural;
   
   -- Tokenizer fonksiyonları
   procedure Get_Next_Token is
      Digit_Value : Integer;
      Digit_Count : Natural := 0;
   begin
      -- Boşlukları atla
      while Current_Position <= Expression_Length and then 
            Expression(Current_Position) = ' ' loop
         Current_Position := Current_Position + 1;
      end loop;
      
      -- Giriş sonuna geldik mi kontrol et
      if Current_Position > Expression_Length then
         Current_Token := (End_Of_Input, 0);
         return;
      end if;
      
      -- Sayı mı kontrol et
      if Expression(Current_Position) in '0'..'9' then
         Digit_Value := 0;
         
         while Current_Position <= Expression_Length and then 
               Expression(Current_Position) in '0'..'9' loop
            Digit_Value := Digit_Value * 10 + (Character'Pos(Expression(Current_Position)) - Character'Pos('0'));
            Current_Position := Current_Position + 1;
            Digit_Count := Digit_Count + 1;
         end loop;
         
         if Digit_Count > 0 then
            Current_Token := (Number, Digit_Value);
            return;
         end if;
      end if;
      
      -- Operatör mü kontrol et
      case Expression(Current_Position) is
         when '+' =>
            Current_Token := (Plus, 0);
            Current_Position := Current_Position + 1;
         when '-' =>
            Current_Token := (Minus, 0);
            Current_Position := Current_Position + 1;
         when '*' =>
            Current_Token := (Multiply, 0);
            Current_Position := Current_Position + 1;
         when '/' =>
            Current_Token := (Divide, 0);
            Current_Position := Current_Position + 1;
         when others =>
            Put_Line("Hata: Geçersiz karakter: " & Expression(Current_Position));
            Current_Token := (End_Of_Input, 0);
      end case;
   end Get_Next_Token;
   
   -- Parser fonksiyonları - İleri bildirimleri
   function Parse_Expression return Integer;
   function Parse_Term return Integer;
   function Parse_Factor return Integer;
   
   -- Parse_Factor fonksiyonu
   function Parse_Factor return Integer is
      Result : Integer;
   begin
      if Current_Token.Kind = Number then
         Result := Current_Token.Value;
         Get_Next_Token;
         return Result;
      else
         Put_Line("Hata: Sayı bekleniyor");
         return 0;
      end if;
   end Parse_Factor;
   
   -- Parse_Term fonksiyonu
   function Parse_Term return Integer is
      Left : Integer;
      Right : Integer;
   begin
      Left := Parse_Factor;
      
      while Current_Token.Kind = Multiply or Current_Token.Kind = Divide loop
         if Current_Token.Kind = Multiply then
            Get_Next_Token;
            Right := Parse_Factor;
            Left := Left * Right;
         elsif Current_Token.Kind = Divide then
            Get_Next_Token;
            Right := Parse_Factor;
            if Right = 0 then
               Put_Line("Hata: Sıfıra bölme hatası");
               return 0;
            else
               Left := Left / Right;
            end if;
         end if;
      end loop;
      
      return Left;
   end Parse_Term;
   
   -- Parse_Expression fonksiyonu
   function Parse_Expression return Integer is
      Left : Integer;
      Right : Integer;
   begin
      Left := Parse_Term;
      
      while Current_Token.Kind = Plus or Current_Token.Kind = Minus loop
         if Current_Token.Kind = Plus then
            Get_Next_Token;
            Right := Parse_Term;
            Left := Left + Right;
         elsif Current_Token.Kind = Minus then
            Get_Next_Token;
            Right := Parse_Term;
            Left := Left - Right;
         end if;
      end loop;
      
      return Left;
   end Parse_Expression;
   
   -- Hesaplama fonksiyonu
   function Evaluate(Expr : String; Len : Natural) return Integer is
   begin
      Expression := (others => ' ');
      Expression(1..Len) := Expr(1..Len);
      Expression_Length := Len;
      Current_Position := 1;
      
      Get_Next_Token;
      return Parse_Expression;
   exception
      when others =>
         Put_Line("Hata: Geçersiz ifade");
         return 0;
   end Evaluate;

begin
   Put_Line("Ada Hesap Makinesi");
   Put_Line("Aritmetik ifadeleri girin (örn: 5+5, 10-4, 3*7, 8/2, 5+5*2, 10/2+3)");
   Put_Line("Çıkmak için 'exit' yazın");
   
   loop
      Put("> ");
      Get_Line(Input, Last);
      
      -- Çıkış kontrolü
      if Input(1..Last) = "exit" then
         exit;
      end if;
      
      -- İfadeyi hesapla
      begin
         declare
            Result : Integer;
         begin
            Result := Evaluate(Input(1..Last), Last);
            Put("= ");
            Put(Result);
            New_Line;
         end;
      exception
         when others =>
            Put_Line("Hata: İşlem sırasında beklenmeyen hata oluştu");
      end;
   end loop;
   
   Put_Line("Hoşçakalın!");
end Calculator; 