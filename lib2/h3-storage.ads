with H3.Storage_Pools;
with System.Pool_Global;

package H3.Storage is

	-- the H3.Storage.Pool_Box is a wrapper that binds a storage pool type 
	-- and an actual storage pool. Other generic packages are desinged to 
	-- accept this single binding package rather than a type and an object 
	-- separately.
	--
	-- generic
	-- 	...
	-- 	type Storage_Pool_Type is new H3.Root_Storage_Pool with private;
	-- 	Storage_Pool: in out Storage_Pool_Type;
	-- 	...
	-- package ... is
	--
	--    <<VS>>
	--
	-- generic
	-- 	...
	-- 	with package Storage_Pool_Box is new H3.Storage.Pool_Box(<>);
	-- 	...
	-- package ... is
	--
	generic
		type Storage_Pool_Type is new H3.Root_Storage_Pool with private;
		Storage_Pool: in out Storage_Pool_Type; -- actual storage pool object.
	package Pool_Box is
		-- blank
	end Pool_Box;

	-- -------------------------------------------------------------
	Global_Pool: H3.Storage_Pools.Global_Pool;

	package Global_Pool_Box is new Pool_Box(
		Storage_Pool_Type => H3.Storage_Pools.Global_Pool,
		Storage_Pool => Global_Pool
	);

	-- -------------------------------------------------------------
	package System_Pool_Box is new Pool_Box(
		Storage_Pool_Type => System.Pool_Global.Unbounded_No_Reclaim_Pool,
		Storage_Pool => System.Pool_Global.Global_Pool_Object
	);

end H3.Storage;
