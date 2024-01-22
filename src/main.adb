-- Filename: Main.adb

with Ada.Text_IO; use Ada.Text_IO;
with Interpreter; use Interpreter;

procedure Main is
    -- Define the variable to store the filename
    Filename : String(1..255);
    Filename_Length : Natural;

    -- Function to check if a file exists
    function File_Exists (Name : String) return Boolean is
        F : File_Type;
    begin
        begin
            Open (F, In_File, Name);
            Close (F);
            return True;
        exception
            when others =>
                return False;
        end;
    end File_Exists;

begin
    -- Get the filename from the user
    Put("Enter the filename: ");
    Get_Line(Filename, Filename_Length);

    -- Check if the file exists
    if File_Exists(Filename) then
        -- Ask the user for the mode
        Put("Do you want to run in normal mode or debug mode? (N/D): ");
        declare
            Mode : Character;
        begin
            Get(Item => Mode);
            Skip_Line;
            case Mode is
            when 'N' =>
                RunNormal (Filename);
            when 'D' =>
                RunDebug (Filename);
            when others =>
                Put_Line("Invalid mode");
            end case;
        end;
    else
        Put_Line("File does not found");
    end if;

end Main;
