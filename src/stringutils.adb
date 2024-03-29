package body StringUtils is

    function Split (Line : in String) return Split_String is
        Lower_Bound : Integer := Line'First;
        Index : Integer := 1;
        Array_Length : Integer := 1;
        Empty : constant Split_String (1 .. 1) := (Others => To_Unbounded_String (""));
    begin
        -- Check if the string is empty
        if Line'Length = 0 then
            return Empty;
        end if;
        -- Count the number of spaces in the string
        for I in Line'Range loop
            if Line (I) = ' ' then
                Array_Length := Array_Length + 1;
            end if;
        end loop;

        -- Create the array
        declare
            Result : Split_String (1 .. Array_Length);
        begin
            -- Split the string
            for I in Line'Range loop
                if Line (I) = ' ' then
                    Result (Index) := To_Unbounded_String (Line (Lower_Bound .. I - 1));
                    Lower_Bound := I + 1;
                    Index := Index + 1;
                end if;
            end loop;
            -- Add the last word
            Result (Index) := To_Unbounded_String (Line (Lower_Bound .. Line'Last));
            return Result;
        end;
    end Split;

end StringUtils;
