
package body H3 is

	function Align (X: in System_Size; Y: in System_Size) return System_Size is
	begin
		return ((X + Y - 1) / Y) * Y;
	end Align;

end H3;
