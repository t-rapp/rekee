var searchIndex = JSON.parse('{\
"rekee":{"doc":"Library providing track editor functions for the Rekee …","i":[[0,"edition","rekee","Set of tiles that belong to the same game edition (core …",null,null],[4,"Edition","rekee::edition","Rallyman game edition (core box or one of the expansions).",null,null],[13,"GtCoreBox","","Rallyman: GT core box",0,null],[13,"GtChampionship","","Championship expansion for Rallyman: GT",0,null],[13,"GtWorldTour","","World Tour expansion for Rallyman: GT",0,null],[13,"GtTeamChallenge","","Team Challenge expansion for Rallyman: GT",0,null],[13,"GtAdrenalinePack","","Adrenaline Pack expansion for Rallyman: GT",0,null],[13,"DirtCoreBox","","Rallyman: DIRT core box",0,null],[13,"Dirt110Percent","","110% expansion for Rallyman: DIRT",0,null],[11,"all_tiles","","Returns all the tiles of the game joined into a sorted …",0,[[],[["vec",3],["tileid",3]]]],[11,"gt_tiles","","Returns tiles part of Rallyman: GT game editions joined …",0,[[],[["vec",3],["tileid",3]]]],[11,"dirt_tiles","","Returns tiles part of Rallyman: DIRT game editions joined …",0,[[],[["vec",3],["tileid",3]]]],[11,"tiles","","Returns the tiles of a specific game edition.",0,[[],[["vec",3],["tileid",3]]]],[11,"iter","","Iterator over all game editions.",0,[[],["iter",3]]],[0,"hexagon","rekee","Types and methods for working with a hexagon grid.",null,null],[3,"Coordinate","rekee::hexagon","Coordinate within a hexagon grid.",null,null],[11,"new","","Creates a new grid coordinate from positions on <code>q</code> and <code>r</code> …",1,[[["i32",15]]]],[11,"q","","Coordinate position on <code>q</code> axis.",1,[[],["i32",15]]],[11,"r","","Coordinate position on <code>r</code> axis.",1,[[],["i32",15]]],[11,"s","","Coordinate position on <code>s</code> axis.",1,[[],["i32",15]]],[11,"neighbor","","Get adjacent grid coordinate based on the given direction.",1,[[]]],[11,"rotated_left","","Get coordinate rotated left (counter-clockwise) by 60° …",1,[[]]],[11,"rotated_right","","Get coordinate rotated right (clockwise) by 60° around …",1,[[]]],[11,"to_pixel","","Convert this hexagon grid coordinate into x/y pixel …",1,[[["layout",3]],["point",3]]],[11,"from_pixel_rounded","","Convert x/y pixel positions back into a hexagon grid …",1,[[["point",3],["layout",3]]]],[4,"Direction","","Direction within a hexagon grid.",null,null],[13,"A","","Direction of the positive <code>q</code> axis.",2,null],[13,"B","","Direction of the positive <code>r</code> axis.",2,null],[13,"C","","Direction of the positive <code>s</code> axis.",2,null],[13,"D","","Direction of the negative <code>q</code> axis.",2,null],[13,"E","","Direction of the negative <code>r</code> axis.",2,null],[13,"F","","Direction of the negative <code>s</code> axis.",2,null],[11,"iter","","Iterator over all six directions.",2,[[],["iter",3]]],[11,"opposite","","Get opposite direction (point reflection).",2,[[]]],[11,"rotated_left","","Get direction rotated left (counter-clockwise).",2,[[]]],[11,"rotated_right","","Get direction rotated right (clockwise).",2,[[]]],[11,"to_angle","","Convert this hexagon grid direction into an angle in …",2,[[["layout",3]],["f32",15]]],[3,"Point","","Position within a grid of rectangular pixels.",null,null],[12,"0","","",3,null],[12,"1","","",3,null],[11,"x","","Position on the <code>x</code> axis.",3,[[],["f32",15]]],[11,"y","","Position on the <code>y</code> axis.",3,[[],["f32",15]]],[3,"Rect","","Rectangle area within a grid of rectangular pixels. It …",null,null],[12,"left","","",4,null],[12,"top","","",4,null],[12,"width","","",4,null],[12,"height","","",4,null],[11,"new","","Creates a new rectangle from the <code>left</code>, <code>top</code>, <code>width</code> and …",4,[[["f32",15]]]],[11,"right","","Right position of the rectangle.",4,[[],["f32",15]]],[11,"bottom","","Bottom position of the rectangle.",4,[[],["f32",15]]],[11,"center","","Center point of the rectangle.",4,[[],["point",3]]],[11,"union","","Calculates the union of two rectangles, the smallest …",4,[[["rect",3]],["rect",3]]],[3,"Orientation","","Hexagon orientation coefficients.",null,null],[11,"pointy","","Pointy topped orientation, hexagons are aligned in …",5,[[]]],[11,"flat","","Flat topped orientation, hexagons are aligned in vertical …",5,[[]]],[3,"Layout","","Coefficients for converting between hexagonal grid …",null,null],[11,"new","","Creates a new grid layout, consisting of hexagon …",6,[[["orientation",3],["point",3]]]],[11,"orientation","","Returns a reference to the grid orientation parameters.",6,[[],["orientation",3]]],[11,"size","","Returns the hexagon size of this grid layout.",6,[[],["point",3]]],[11,"origin","","Returns the grid origin point.",6,[[],["point",3]]],[11,"is_pointy","","Returns whether the hexagon orientation is \\\"pointy top\\\" …",6,[[],["bool",15]]],[11,"is_flat","","Returns whether the hexagon orientation is \\\"flat top\\\" or …",6,[[],["bool",15]]],[11,"hexagon_corners","","Calculates the corners of a hexagon with the given …",6,[[["coordinate",3]]]],[11,"hexagon_rect","","Calculates the rectangular box of a hexagon with the …",6,[[["coordinate",3]],["rect",3]]],[0,"import","rekee","Import and export of map data.",null,null],[5,"import_rgt","rekee::import","",null,[[["str",15]],[["result",6],["map",3]]]],[5,"export_rgt","","",null,[[["map",3]],[["result",6],["string",3]]]],[5,"build_file_name","","",null,[[["str",15]],["string",3]]],[0,"map","rekee","Map of tiles within a hexagon grid.",null,null],[3,"PlacedTile","rekee::map","Single tile that is part of the map. A placed tile …",null,null],[12,"pos","","",7,null],[12,"dir","","",7,null],[11,"new","","Creates a new tile with identifier, coordinates, and …",7,[[["coordinate",3],["tileid",3],["direction",4]]]],[11,"id","","Tile identifier.",7,[[],["tileid",3]]],[3,"Map","","Map for storing track tiles.",null,null],[11,"new","","Creates a new and empty map.",8,[[]]],[11,"title","","Map title.",8,[[],["str",15]]],[11,"set_title","","Updates the map title.",8,[[["str",15]]]],[11,"tiles","","List of all tiles placed on the map.",8,[[]]],[11,"active_pos","","Active position for the next tile <code>append</code> action. Returns …",8,[[],[["coordinate",3],["option",4]]]],[11,"set_active_pos","","Update position for the next tile <code>append</code> action.",8,[[["coordinate",3]]]],[11,"active_dir","","Active direction for the next tile <code>append</code> action.",8,[[],["direction",4]]],[11,"get","","Returns tile at the given map position, if existing.",8,[[["coordinate",3]],[["placedtile",3],["option",4]]]],[11,"insert","","Insert a new tile using the specified position and …",8,[[["coordinate",3],["tileid",3],["direction",4]]]],[11,"append","","Append a new tile to the map using the active position. …",8,[[["option",4],["option",4],["coordinate",3],["tileid",3],["connectionhint",4]],["bool",15]]],[11,"remove","","Remove tile at the given position.",8,[[["coordinate",3]]]],[11,"align_center","","Re-align all tiles around the map center.",8,[[]]],[11,"rotate_left","","Rotate all tile positions to the left (counter-clockwise).",8,[[]]],[11,"rotate_right","","Rotate all tile positions to the right (clockwise).",8,[[]]],[0,"tile","rekee","Tile identifier and characteristics.",null,null],[3,"TileId","rekee::tile","Identifier of a game tile.",null,null],[11,"new","","Creates a new tile identifier from catalog number, tile …",9,[[["u16",15],["u8",15]]]],[11,"num","","Tile catalog number.",9,[[],["u16",15]]],[11,"side","","Tile side (1 => front / a, 2 => back / b).",9,[[],["u8",15]]],[11,"var","","Graphical tile variant.",9,[[],["u8",15]]],[11,"base","","Returns the \\\"base\\\" identifier of a game tile. This is the …",9,[[]]],[4,"ConnectionHint","","Tile connection hint. Used to describe the orientation of …",null,null],[13,"Straight","","",10,null],[13,"Left","","",10,null],[13,"Right","","",10,null],[4,"ParseHintError","","",null,null],[13,"Unknown","","",11,null],[4,"Connection","","Internal tile connection information. Describes how a …",null,null],[13,"None","","",12,null],[13,"Straight","","",12,null],[13,"Left","","",12,null],[13,"Right","","",12,null],[13,"Junction","","",12,null],[11,"target","","",12,[[["direction",4]],[["direction",4],["option",4]]]],[4,"Edge","","External tile edge information. Describes how the track …",null,null],[13,"None","","",13,null],[13,"Straight","","",13,null],[13,"SkewLeft","","",13,null],[13,"SkewRight","","",13,null],[11,"lanes","","Lane count of the tile edge.",13,[[],["u8",15]]],[4,"Terrain","","Tile terrain information. Describes the track surface of …",null,null],[13,"None","","",14,null],[13,"Asphalt","","",14,null],[13,"Gravel","","",14,null],[13,"Snow","","",14,null],[11,"danger_level","","Danger level of a tile.",14,[[],["u8",15]]],[4,"ParseTerrainError","","",null,null],[13,"UnknownSurface","","",15,null],[13,"InvalidDangerLevel","","",15,null],[3,"TileInfo","","Information about tile characteristics like graphical …",null,null],[11,"base_id","","Base identifier of the corresponding game tile.",16,[[],["tileid",3]]],[11,"full_id","","Full identifier of the corresponding game tile. If the …",16,[[],["tileid",3]]],[11,"iter","","Returns an interator over all tiles in the internal list.",16,[[],["iter",3]]],[11,"get","","Lookup tile information by tile identifier. The …",16,[[["tileid",3]],["option",4]]],[11,"count","","Number of graphical variants that are available for a …",16,[[],["usize",15]]],[11,"terrain","","Terrain information for a tile.",16,[[],["terrain",4]]],[11,"connection","","Connection information for one of the six directions of a …",16,[[["direction",4]],["connection",4]]],[11,"connection_target","","Connection target direction for one of the six directions …",16,[[["direction",4]],[["direction",4],["option",4]]]],[11,"edge","","Edge informantion for one of the six directions of a tile.",16,[[["direction",4]],["edge",4]]],[5,"main","rekee","",null,[[],[["result",4],["jsvalue",3]]]],[5,"__wasm_bindgen_generated_main","","",null,[[]]],[14,"warn","","Prints a warning message to the log output.",null,null],[14,"log","","Prints a generic message to the log output.",null,null],[14,"info","","Prints an info message to the log output.",null,null],[14,"debug","","Prints a debug message to the log output.",null,null],[14,"tile","","Creates a new tile identifier.",null,null],[11,"from","rekee::edition","",0,[[]]],[11,"into","","",0,[[]]],[11,"borrow","","",0,[[]]],[11,"borrow_mut","","",0,[[]]],[11,"try_from","","",0,[[],["result",4]]],[11,"try_into","","",0,[[],["result",4]]],[11,"type_id","","",0,[[],["typeid",3]]],[11,"from","rekee::hexagon","",1,[[]]],[11,"into","","",1,[[]]],[11,"to_owned","","",1,[[]]],[11,"clone_into","","",1,[[]]],[11,"to_string","","",1,[[],["string",3]]],[11,"borrow","","",1,[[]]],[11,"borrow_mut","","",1,[[]]],[11,"try_from","","",1,[[],["result",4]]],[11,"try_into","","",1,[[],["result",4]]],[11,"type_id","","",1,[[],["typeid",3]]],[11,"from","","",2,[[]]],[11,"into","","",2,[[]]],[11,"to_owned","","",2,[[]]],[11,"clone_into","","",2,[[]]],[11,"to_string","","",2,[[],["string",3]]],[11,"borrow","","",2,[[]]],[11,"borrow_mut","","",2,[[]]],[11,"try_from","","",2,[[],["result",4]]],[11,"try_into","","",2,[[],["result",4]]],[11,"type_id","","",2,[[],["typeid",3]]],[11,"from","","",3,[[]]],[11,"into","","",3,[[]]],[11,"to_owned","","",3,[[]]],[11,"clone_into","","",3,[[]]],[11,"borrow","","",3,[[]]],[11,"borrow_mut","","",3,[[]]],[11,"try_from","","",3,[[],["result",4]]],[11,"try_into","","",3,[[],["result",4]]],[11,"type_id","","",3,[[],["typeid",3]]],[11,"from","","",4,[[]]],[11,"into","","",4,[[]]],[11,"to_owned","","",4,[[]]],[11,"clone_into","","",4,[[]]],[11,"borrow","","",4,[[]]],[11,"borrow_mut","","",4,[[]]],[11,"try_from","","",4,[[],["result",4]]],[11,"try_into","","",4,[[],["result",4]]],[11,"type_id","","",4,[[],["typeid",3]]],[11,"from","","",5,[[]]],[11,"into","","",5,[[]]],[11,"to_owned","","",5,[[]]],[11,"clone_into","","",5,[[]]],[11,"borrow","","",5,[[]]],[11,"borrow_mut","","",5,[[]]],[11,"try_from","","",5,[[],["result",4]]],[11,"try_into","","",5,[[],["result",4]]],[11,"type_id","","",5,[[],["typeid",3]]],[11,"from","","",6,[[]]],[11,"into","","",6,[[]]],[11,"to_owned","","",6,[[]]],[11,"clone_into","","",6,[[]]],[11,"borrow","","",6,[[]]],[11,"borrow_mut","","",6,[[]]],[11,"try_from","","",6,[[],["result",4]]],[11,"try_into","","",6,[[],["result",4]]],[11,"type_id","","",6,[[],["typeid",3]]],[11,"from","rekee::map","",7,[[]]],[11,"into","","",7,[[]]],[11,"to_owned","","",7,[[]]],[11,"clone_into","","",7,[[]]],[11,"borrow","","",7,[[]]],[11,"borrow_mut","","",7,[[]]],[11,"try_from","","",7,[[],["result",4]]],[11,"try_into","","",7,[[],["result",4]]],[11,"type_id","","",7,[[],["typeid",3]]],[11,"from","","",8,[[]]],[11,"into","","",8,[[]]],[11,"to_owned","","",8,[[]]],[11,"clone_into","","",8,[[]]],[11,"borrow","","",8,[[]]],[11,"borrow_mut","","",8,[[]]],[11,"try_from","","",8,[[],["result",4]]],[11,"try_into","","",8,[[],["result",4]]],[11,"type_id","","",8,[[],["typeid",3]]],[11,"from","rekee::tile","",9,[[]]],[11,"into","","",9,[[]]],[11,"to_owned","","",9,[[]]],[11,"clone_into","","",9,[[]]],[11,"to_string","","",9,[[],["string",3]]],[11,"borrow","","",9,[[]]],[11,"borrow_mut","","",9,[[]]],[11,"try_from","","",9,[[],["result",4]]],[11,"try_into","","",9,[[],["result",4]]],[11,"type_id","","",9,[[],["typeid",3]]],[11,"from","","",10,[[]]],[11,"into","","",10,[[]]],[11,"to_owned","","",10,[[]]],[11,"clone_into","","",10,[[]]],[11,"to_string","","",10,[[],["string",3]]],[11,"borrow","","",10,[[]]],[11,"borrow_mut","","",10,[[]]],[11,"try_from","","",10,[[],["result",4]]],[11,"try_into","","",10,[[],["result",4]]],[11,"type_id","","",10,[[],["typeid",3]]],[11,"from","","",11,[[]]],[11,"into","","",11,[[]]],[11,"to_string","","",11,[[],["string",3]]],[11,"borrow","","",11,[[]]],[11,"borrow_mut","","",11,[[]]],[11,"try_from","","",11,[[],["result",4]]],[11,"try_into","","",11,[[],["result",4]]],[11,"type_id","","",11,[[],["typeid",3]]],[11,"from","","",12,[[]]],[11,"into","","",12,[[]]],[11,"to_owned","","",12,[[]]],[11,"clone_into","","",12,[[]]],[11,"borrow","","",12,[[]]],[11,"borrow_mut","","",12,[[]]],[11,"try_from","","",12,[[],["result",4]]],[11,"try_into","","",12,[[],["result",4]]],[11,"type_id","","",12,[[],["typeid",3]]],[11,"from","","",13,[[]]],[11,"into","","",13,[[]]],[11,"to_owned","","",13,[[]]],[11,"clone_into","","",13,[[]]],[11,"borrow","","",13,[[]]],[11,"borrow_mut","","",13,[[]]],[11,"try_from","","",13,[[],["result",4]]],[11,"try_into","","",13,[[],["result",4]]],[11,"type_id","","",13,[[],["typeid",3]]],[11,"from","","",14,[[]]],[11,"into","","",14,[[]]],[11,"to_owned","","",14,[[]]],[11,"clone_into","","",14,[[]]],[11,"borrow","","",14,[[]]],[11,"borrow_mut","","",14,[[]]],[11,"try_from","","",14,[[],["result",4]]],[11,"try_into","","",14,[[],["result",4]]],[11,"type_id","","",14,[[],["typeid",3]]],[11,"from","","",15,[[]]],[11,"into","","",15,[[]]],[11,"to_string","","",15,[[],["string",3]]],[11,"borrow","","",15,[[]]],[11,"borrow_mut","","",15,[[]]],[11,"try_from","","",15,[[],["result",4]]],[11,"try_into","","",15,[[],["result",4]]],[11,"type_id","","",15,[[],["typeid",3]]],[11,"from","","",16,[[]]],[11,"into","","",16,[[]]],[11,"to_string","","",16,[[],["string",3]]],[11,"borrow","","",16,[[]]],[11,"borrow_mut","","",16,[[]]],[11,"try_from","","",16,[[],["result",4]]],[11,"try_into","","",16,[[],["result",4]]],[11,"type_id","","",16,[[],["typeid",3]]],[11,"from","rekee::hexagon","",1,[[],["coordinate",3]]],[11,"from","","",2,[[["u8",15]],["direction",4]]],[11,"from","","",2,[[["i8",15]],["direction",4]]],[11,"from","","",2,[[["i32",15]],["direction",4]]],[11,"from","","",3,[[],["point",3]]],[11,"clone","","",1,[[],["coordinate",3]]],[11,"clone","","",2,[[],["direction",4]]],[11,"clone","","",3,[[],["point",3]]],[11,"clone","","",4,[[],["rect",3]]],[11,"clone","","",5,[[],["orientation",3]]],[11,"clone","","",6,[[],["layout",3]]],[11,"clone","rekee::map","",7,[[],["placedtile",3]]],[11,"clone","","",8,[[],["map",3]]],[11,"clone","rekee::tile","",9,[[],["tileid",3]]],[11,"clone","","",10,[[],["connectionhint",4]]],[11,"clone","","",12,[[],["connection",4]]],[11,"clone","","",13,[[],["edge",4]]],[11,"clone","","",14,[[],["terrain",4]]],[11,"default","rekee::hexagon","",1,[[],["coordinate",3]]],[11,"default","","",3,[[],["point",3]]],[11,"default","","",4,[[],["rect",3]]],[11,"default","","",6,[[]]],[11,"default","rekee::map","",8,[[]]],[11,"default","rekee::tile","",9,[[],["tileid",3]]],[11,"default","","",12,[[]]],[11,"default","","",13,[[]]],[11,"default","","",14,[[]]],[11,"default","","",16,[[],["tileinfo",3]]],[11,"cmp","rekee::hexagon","",1,[[["coordinate",3]],["ordering",4]]],[11,"cmp","rekee::tile","",9,[[["tileid",3]],["ordering",4]]],[11,"eq","rekee::hexagon","",1,[[["coordinate",3]],["bool",15]]],[11,"ne","","",1,[[["coordinate",3]],["bool",15]]],[11,"eq","","",2,[[["direction",4]],["bool",15]]],[11,"eq","","",3,[[["point",3]],["bool",15]]],[11,"ne","","",3,[[["point",3]],["bool",15]]],[11,"eq","","",4,[[["rect",3]],["bool",15]]],[11,"ne","","",4,[[["rect",3]],["bool",15]]],[11,"eq","rekee::map","",7,[[["placedtile",3]],["bool",15]]],[11,"eq","rekee::tile","",9,[[["tileid",3]],["bool",15]]],[11,"ne","","",9,[[["tileid",3]],["bool",15]]],[11,"eq","","",10,[[["connectionhint",4]],["bool",15]]],[11,"eq","","",11,[[["parsehinterror",4]],["bool",15]]],[11,"ne","","",11,[[["parsehinterror",4]],["bool",15]]],[11,"eq","","",12,[[["connection",4]],["bool",15]]],[11,"ne","","",12,[[["connection",4]],["bool",15]]],[11,"eq","","",12,[[["connectionhint",4]],["bool",15]]],[11,"eq","","",13,[[["edge",4]],["bool",15]]],[11,"ne","","",13,[[["edge",4]],["bool",15]]],[11,"eq","","",14,[[["terrain",4]],["bool",15]]],[11,"ne","","",14,[[["terrain",4]],["bool",15]]],[11,"eq","","",15,[[["parseterrainerror",4]],["bool",15]]],[11,"ne","","",15,[[["parseterrainerror",4]],["bool",15]]],[11,"partial_cmp","rekee::hexagon","",1,[[["coordinate",3]],[["option",4],["ordering",4]]]],[11,"lt","","",1,[[["coordinate",3]],["bool",15]]],[11,"le","","",1,[[["coordinate",3]],["bool",15]]],[11,"gt","","",1,[[["coordinate",3]],["bool",15]]],[11,"ge","","",1,[[["coordinate",3]],["bool",15]]],[11,"partial_cmp","rekee::tile","",9,[[["tileid",3]],[["option",4],["ordering",4]]]],[11,"lt","","",9,[[["tileid",3]],["bool",15]]],[11,"le","","",9,[[["tileid",3]],["bool",15]]],[11,"gt","","",9,[[["tileid",3]],["bool",15]]],[11,"ge","","",9,[[["tileid",3]],["bool",15]]],[11,"fmt","rekee::hexagon","",1,[[["formatter",3]],["result",6]]],[11,"fmt","","",2,[[["formatter",3]],["result",6]]],[11,"fmt","","",3,[[["formatter",3]],["result",6]]],[11,"fmt","","",4,[[["formatter",3]],["result",6]]],[11,"fmt","","",5,[[["formatter",3]],["result",6]]],[11,"fmt","","",6,[[["formatter",3]],["result",6]]],[11,"fmt","rekee::map","",7,[[["formatter",3]],["result",6]]],[11,"fmt","","",8,[[["formatter",3]],["result",6]]],[11,"fmt","rekee::tile","",9,[[["formatter",3]],["result",6]]],[11,"fmt","","",10,[[["formatter",3]],["result",6]]],[11,"fmt","","",11,[[["formatter",3]],["result",6]]],[11,"fmt","","",12,[[["formatter",3]],["result",6]]],[11,"fmt","","",13,[[["formatter",3]],["result",6]]],[11,"fmt","","",14,[[["formatter",3]],["result",6]]],[11,"fmt","","",15,[[["formatter",3]],["result",6]]],[11,"fmt","","",16,[[["formatter",3]],["result",6]]],[11,"fmt","rekee::hexagon","",1,[[["formatter",3]],["result",6]]],[11,"fmt","","",2,[[["formatter",3]],["result",6]]],[11,"fmt","rekee::tile","",9,[[["formatter",3]],["result",6]]],[11,"fmt","","",10,[[["formatter",3]],["result",6]]],[11,"fmt","","",11,[[["formatter",3]],["result",6]]],[11,"fmt","","",15,[[["formatter",3]],["result",6]]],[11,"fmt","","",16,[[["formatter",3]],["result",6]]],[11,"sub","rekee::hexagon","",1,[[["coordinate",3]],["coordinate",3]]],[11,"sub","","",2,[[["direction",4]],["direction",4]]],[11,"sub","","",3,[[["point",3]],["point",3]]],[11,"add","","",1,[[["coordinate",3]],["coordinate",3]]],[11,"add","","",2,[[["direction",4]],["direction",4]]],[11,"add","","",3,[[["point",3]],["point",3]]],[11,"hash","rekee::tile","",9,[[]]],[11,"from_str","","",9,[[["str",15]],["result",4]]],[11,"from_str","","",10,[[["str",15]],["result",4]]],[11,"from_str","","",14,[[["str",15]],["result",4]]],[11,"serialize","rekee::hexagon","",1,[[],["result",4]]],[11,"serialize","","",2,[[],["result",4]]],[11,"serialize","rekee::map","",7,[[],["result",4]]],[11,"serialize","","",8,[[],["result",4]]],[11,"serialize","rekee::tile","",9,[[],["result",4]]],[11,"deserialize","rekee::hexagon","",1,[[],["result",4]]],[11,"deserialize","","",2,[[],[["direction",4],["result",4]]]],[11,"deserialize","rekee::map","",7,[[],[["result",4],["placedtile",3]]]],[11,"deserialize","","",8,[[],["result",4]]],[11,"deserialize","rekee::tile","",9,[[],[["result",4],["tileid",3]]]]],"p":[[4,"Edition"],[3,"Coordinate"],[4,"Direction"],[3,"Point"],[3,"Rect"],[3,"Orientation"],[3,"Layout"],[3,"PlacedTile"],[3,"Map"],[3,"TileId"],[4,"ConnectionHint"],[4,"ParseHintError"],[4,"Connection"],[4,"Edge"],[4,"Terrain"],[4,"ParseTerrainError"],[3,"TileInfo"]]}\
}');
addSearchOptions(searchIndex);initSearch(searchIndex);