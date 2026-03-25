def pretty_print(tree):
    if tree.root is None: return str(None)
    key_length = 1      # max length of a printed key
    small_space = 1     # space between node siblings on bottom row
    big_space = 2      # space between pairs of siblings on bottom row
    
    # assemble list of lists of all keys, by row
    all_keys = []
    node_row = [tree.root]
    while node_row != [None] * len(node_row):
        key_row = []
        for node in node_row:
            if node:
                key_row.append("%s%s" % (node.color, node.key) if node.key is not None else ".")
            else:
                key_row.append(None)
        all_keys.append(key_row)
        new_node_row = []
        for node in node_row:
            new_nodes = (node.left, node.right) if node else (None, None)
            new_node_row.extend(new_nodes)
        node_row = new_node_row
    
    # assemble list of lists of coordinates. These are coordinates where the printed keys will be centered. Bottom row is first calculated using small_space and big_space values; higher rows are adjusted as midpoints
    all_keys.reverse()
    all_coords = []
    pos = key_length//2
    current_coord = []
    # bottom row:
    for j in range(0, len(all_keys[0]), 2):
        current_coord.extend((pos, pos + key_length + small_space))
        pos += small_space + big_space + 2*(key_length)
    all_coords.append(current_coord)
    
    for i in range(1, len(all_keys)):
        current_coord = []
        for j in range(0, len(all_keys[i-1]), 2):
            current_coord.append(int((all_coords[i-1][j]+all_coords[i-1][j+1])/2))
        all_coords.append(current_coord)
    all_keys.reverse()
    all_coords.reverse()
    
    # assemble output string. For each row, first print initial blank space, then each node followed by the next coordinate difference of blanks, followed by the last node.
    output = ''
    output_line = ''
    for i in range(len(all_keys)):
        output_line = ' '*(all_coords[i][0] - key_length//2)
        for j in range(len(all_keys[i])-1):
            key_str = str(all_keys[i][j]) if all_keys[i][j] is not None else '*'
            output_line += key_str + ' '*(key_length - len(key_str)) #print the key; pad it with blanks to get up to key_length chars
            output_line += ' '*(all_coords[i][j+1] - all_coords[i][j] - key_length)
        output_line += str(all_keys[i][-1]) if all_keys[i][-1] is not None else '*'
        output += (output_line + '\n')
    print(output)
