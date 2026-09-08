function node_id_to_symbol(kind::Symbol)::Char
    if kind == :and return 'A'
    elseif kind == :or return 'O'
    elseif kind == :not return 'N'
    elseif kind == :nand return 'Y'  # Y for "yes, it's not and"
    elseif kind == :nor return 'R'   # R for "really not or"
    elseif kind == :sum return 'S'
    elseif kind == :avg return 'V'   # V for "average"
    elseif kind == :product return 'P'
    elseif kind == :diff return 'D'
    elseif kind == :max return 'X'   # eXtra big
    elseif kind == :min return 'I'   # Infinitesimal
    else return '?' 
    end
end

function symbol_to_node_kind(symbol::Char)::Symbol
    if symbol == 'A' return :and
    elseif symbol == 'O' return :or
    elseif symbol == 'N' return :not
    elseif symbol == 'Y' return :nand
    elseif symbol == 'R' return :nor
    elseif symbol == 'S' return :sum
    elseif symbol == 'V' return :avg
    elseif symbol == 'P' return :product
    elseif symbol == 'D' return :diff
    elseif symbol == 'X' return :max
    elseif symbol == 'I' return :min
    else return :and  # safe default
    end
end

function is_gate_symbol(symbol::Char)::Bool
    return symbol in ['A', 'O', 'N', 'Y', 'R', 'S', 'V', 'P', 'D', 'X', 'I']
end

function is_module_marker(symbol::Char)::Bool
    return symbol in ['M', 'K']
end

abstract type AbstractNode end

# Basic nodes
struct Sensor <: AbstractNode
  id::Int
  value::Float64
  name::String
end

struct Actuator <: AbstractNode
  id::Int
  value::Float64
  name::String
end

struct Gate <: AbstractNode
  id:Int
  kind:Symbol
  # :and, :or, :not, :nand, :sum, :product, :diff, :max, :min
  inputs::Vector{Int}value::Float64
  name::String
end

# 'Call' gate - calls a module
struct CallGate <: AbstractNode
  id::Int
  target_module::Int
  input_mappings::Vector{Pair{Int,Int}}
  # (local_node_id, module_input_index)
  value::Float64
  name::String
end

# Modules are defined as blueprints
struct ModuleDef
  id::Int
  is_memory::Bool
  decay::Float64 # used iff is_memory
  genes::Vector{Union{Int, Char}}
  # raw genome fragment filled when module first used
  circuit::Union{Nothing, Circuit}
  input_count::Int
  output_node::Int
  state:Float64 # for memory modules
end

# A memory gate (K) creates a MemoryModule
struct MemoryModule <: AbstractNode
    id::Int
    module_id::Int  # which module definition to use
    decay::Float64
    state::Float64
    inputs::Vector{Int}
    value::Float64
    name::String
end

# Connections are wires
struct Connection
    from::Int
    to::Int
end

# A Circuit is a complete brain
struct Circuit
    nodes::Dict{Int, AbstractNode}
    ordered_nodes::Vector{Int}
    connections::Vector{Connection}
    eval_order::Vector{Int}
    module_defs::Dict{Int, ModuleDef}
    main_module::Int  # module ID that runs first
end

function parse_genome(genome::Vector{Union{Int,Char}}, 
                      sensors::Dict{Int,Sensor}, 
                      actuators::Dict{Int,Actuator})::Circuit
    
    # --- STEP 1: Handle implicit first module ---
    # If no M symbols, the ENTIRE genome is Module 1
    if !any(x -> x isa Char && x == 'M', genome)
        # Wrap the whole genome in an implicit M
        modified_genome = vcat(['M'], genome)
    else
        modified_genome = genome
    end
    
    # --- STEP 2: Parse raw genome into module definitions ---
    
    module_defs = Dict{Int, ModuleDef}()
    current_genes = Vector{Union{Int,Char}}()
    next_module_id = 1
    current_is_memory = false
    current_decay = 0.9
    
    # Helper to save current module
    function save_module!()
        # Always save if we have genes OR if we already have modules
        # (this handles empty modules from multiple M's in a row)
        if !isempty(current_genes) || !isempty(module_defs)
            # If this is a memory module, the first number is decay
            if current_is_memory
                # Find first number in genes (should be decay)
                decay_idx = findfirst(x -> x isa Int, current_genes)
                if decay_idx !== nothing
                    current_decay = Float64(current_genes[decay_idx])
                    # Remove decay from genes
                    deleteat!(current_genes, decay_idx)
                end
            end
            
            # Create ModuleDef
            mod_def = ModuleDef(
                next_module_id,
                current_is_memory,
                current_decay,
                current_genes,
                nothing,  # circuit not parsed yet
                0,        # input_count will be computed later
                0,        # output_node will be computed later
                0.5       # initial state for memory
            )
            module_defs[next_module_id] = mod_def
            next_module_id += 1
            current_genes = Vector{Union{Int,Char}}()
            current_is_memory = false
            current_decay = 0.9
        end
    end
    
    # --- Parse the genome ---
    
    i = 1
    while i <= length(modified_genome)
        symbol = modified_genome[i]
        
        if is_module_marker(symbol)
            # Save current module
            save_module!()
            
            # Start new module
            if symbol == 'M'
                current_is_memory = false
            elseif symbol == 'K'
                current_is_memory = true
                # The next symbol should be decay (number)
                if i + 1 <= length(modified_genome) && modified_genome[i+1] isa Int
                    current_decay = Float64(modified_genome[i+1])
                    i += 1  # skip the decay number
                end
            end
            i += 1
            continue
        else
            # Add to current module's genes
            push!(current_genes, symbol)
            i += 1
        end
    end
    
    # Save the last module
    save_module!()
    
    # --- STEP 3: Build the circuit from module definitions ---
    
    # Start with sensors and actuators
    nodes = Dict{Int, AbstractNode}(sensors)
    for (id, actuator) in actuators
        nodes[id] = actuator
    end
    ordered_nodes = [keys(sensors)..., keys(actuators)...]
    connections = Vector{Connection}()
    next_node_id = maximum(keys(nodes)) + 1
    
    # Create the Circuit object
    circuit = Circuit(
        nodes,
        ordered_nodes,
        connections,
        Vector{Int}(),  # eval_order will be computed later
        module_defs,
        1  # main module is always ID 1
    )
    
    # --- STEP 4: Make sure main module exists ---
    
    if !haskey(module_defs, 1)
        # Create empty main module (happens if genome was just "M" or empty)
        module_defs[1] = ModuleDef(1, false, 0.9, [], nothing, 0, 0, 0.5)
    end
    
    # --- STEP 5: Parse the main module immediately ---
    
    parse_module!(circuit, 1)
    
    # --- STEP 6: Compute evaluation order ---
    
    compute_eval_order!(circuit)
    
    return circuit
end

function parse_module!(circuit::Circuit, module_id::Int)
    mod_def = circuit.module_defs[module_id]
    
    # Don't re-parse if already parsed
    if mod_def.circuit !== nothing
        return mod_def.circuit
    end
    
    # Create a temporary circuit for this module
    # It starts with input nodes (placeholders) and will add gates
    temp_nodes = Dict{Int, AbstractNode}()
    temp_ordered = Vector{Int}()
    temp_connections = Vector{Connection}()
    next_node_id = 1
    
    # The first thing: create input nodes
    # We don't know how many inputs yet, so we'll count them as we go
    input_nodes = Vector{Int}()
    
    # --- Parse the genes ---
    
    genes = mod_def.genes
    i = 1
    mode = :build_gate
    number_buffer = Vector{Int}()
    input_count = 0
    
    while i <= length(genes)
        symbol = genes[i]
        
        if symbol isa Char && is_gate_symbol(symbol)
            # Build a gate
            kind = symbol_to_node_kind(symbol)
            
            if kind == :call
                # This is a C gate - call a module
                # Next symbol should be target module ID
                if i + 1 <= length(genes) && genes[i+1] isa Int
                    target_id = genes[i+1]
                    i += 1
                    
                    # Make sure target module exists
                    if !haskey(circuit.module_defs, target_id)
                        # Create placeholder empty module
                        circuit.module_defs[target_id] = ModuleDef(
                            target_id, false, 0.9, [], nothing, 0, 0, 0.5
                        )
                    end
                    
                    # Get target's input count
                    target_def = circuit.module_defs[target_id]
                    target_input_count = target_def.input_count
                    
                    # If target hasn't been parsed yet, parse it now
                    if target_def.circuit === nothing
                        parse_module!(circuit, target_id)
                    end
                    
                    # Read input mappings: pairs of (local_node, module_input_index)
                    mappings = Vector{Pair{Int, Int}}()
                    for j in 1:target_input_count
                        if i + 1 <= length(genes) && genes[i+1] isa Int && genes[i+2] isa Int
                            local_node = genes[i+1]
                            module_input = genes[i+2]
                            push!(mappings, local_node => module_input)
                            i += 2
                        else
                            # Not enough numbers - use defaults
                            push!(mappings, 0 => j)
                        end
                    end
                    
                    # Create CallGate node
                    call_gate = CallGate(
                        next_node_id,
                        target_id,
                        mappings,
                        0.5,
                        "call_$target_id"
                    )
                    temp_nodes[next_node_id] = call_gate
                    push!(temp_ordered, next_node_id)
                    next_node_id += 1
                end
            else
                # Regular gate (AND, OR, NOT, etc.)
                gate = Gate(
                    next_node_id,
                    kind,
                    Vector{Int}(),  # inputs filled later by connections
                    0.5,
                    string(kind)
                )
                temp_nodes[next_node_id] = gate
                push!(temp_ordered, next_node_id)
                next_node_id += 1
            end
            mode = :collect_numbers
            i += 1
            
        elseif symbol isa Int
            # Number - part of connections
            if mode == :collect_numbers
                push!(number_buffer, symbol)
                if length(number_buffer) == 2
                    # Make a connection
                    total_nodes = length(temp_ordered)
                    if total_nodes > 0
                        from_idx = (number_buffer[1] % total_nodes) + 1
                        to_idx = (number_buffer[2] % total_nodes) + 1
                        from_id = temp_ordered[from_idx]
                        to_id = temp_ordered[to_idx]
                        push!(temp_connections, Connection(from_id, to_id))
                        
                        # Add to gate's inputs
                        if haskey(temp_nodes, to_id) && temp_nodes[to_id] isa Gate
                            gate = temp_nodes[to_id]::Gate
                            push!(gate.inputs, from_id)
                        end
                    end
                    empty!(number_buffer)
                end
            end
            i += 1
        else
            i += 1
        end
    end
    
    # --- Build the module circuit ---
    
    # Determine input count (nodes with no incoming connections from inside module)
    input_connections = Set{Int}()
    for conn in temp_connections
        push!(input_connections, conn.to)
    end
    
    # Nodes that are connected to FROM outside are inputs
    # For now, assume the first N nodes that have no incoming connections are inputs
    input_nodes = Vector{Int}()
    for node_id in temp_ordered
        if !(node_id in input_connections) && !(temp_nodes[node_id] isa Actuator)
            push!(input_nodes, node_id)
        end
    end
    input_count = length(input_nodes)
    
    # Output node is the last node in eval order
    output_node = isempty(temp_ordered) ? 0 : temp_ordered[end]
    
    # Create the sub-circuit
    sub_circuit = Circuit(
        temp_nodes,
        temp_ordered,
        temp_connections,
        Vector{Int}(),  # eval_order to be computed
        Dict{Int, ModuleDef}(),  # modules don't nest for now
        0
    )
    
    # Compute eval order for sub-circuit
    compute_eval_order!(sub_circuit)
    
    # Store parsed circuit back in module definition
    mod_def.circuit = sub_circuit
    mod_def.input_count = input_count
    mod_def.output_node = output_node
    
    # If this module is the main module, merge its nodes into the main circuit
    if module_id == 1
        # Add all nodes from main module to circuit
        for (id, node) in temp_nodes
            circuit.nodes[id] = node
            push!(circuit.ordered_nodes, id)
        end
        for conn in temp_connections
            push!(circuit.connections, conn)
        end
    end
    
    return sub_circuit
end

function compute_eval_order!(circuit::Circuit)
    # Build dependency graph
    in_degree = Dict{Int, Int}()
    graph = Dict{Int, Vector{Int}}()
    
    # Initialize all nodes with 0 in-degree
    for id in circuit.ordered_nodes
        in_degree[id] = 0
        graph[id] = Vector{Int}()
    end
    
    # Build edges: from -> to
    for conn in circuit.connections
        if haskey(graph, conn.from)
            push!(graph[conn.from], conn.to)
            in_degree[conn.to] = get(in_degree, conn.to, 0) + 1
        end
    end
    
    # Kahn's algorithm
    queue = [id for (id, deg) in in_degree if deg == 0]
    eval_order = Vector{Int}()
    
    while !isempty(queue)
        id = popfirst!(queue)
        push!(eval_order, id)
        
        for neighbor in get(graph, id, [])
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0
                push!(queue, neighbor)
            end
        end
    end
    
    # If there are cycles, just append remaining nodes
    for id in circuit.ordered_nodes
        if !(id in eval_order)
            push!(eval_order, id)
        end
    end
    
    circuit.eval_order = eval_order
end

function evaluate!(circuit::Circuit)
    # Run evaluation for main circuit
    for node_id in circuit.eval_order
        node = circuit.nodes[node_id]
        
        if node isa Sensor
            # Value already set from environment
            continue
        elseif node isa Actuator
            # Value gets set by gates feeding into it
            continue
        elseif node isa Gate
            # Collect input values
            input_values = [circuit.nodes[id].value for id in node.inputs]
            node.value = evaluate_gate(node.kind, input_values)
        elseif node isa CallGate
            # Call the target module
            node.value = evaluate_call!(circuit, node)
        elseif node isa MemoryModule
            # Handle memory module
            input_values = [circuit.nodes[id].value for id in node.inputs]
            input = isempty(input_values) ? 0.5 : input_values[1]
            node.state = node.decay * node.state + (1 - node.decay) * input
            node.value = node.state
        end
    end
end

function evaluate_gate(kind::Symbol, input_values::Vector{Float64})::Float64
    if isempty(input_values)
        return 0.5  # Neutral default
    end
    
    if kind == :and
        return minimum(input_values)
    elseif kind == :or
        return maximum(input_values)
    elseif kind == :not
        return 1 - input_values[1]  # Use first input only
    elseif kind == :nand
        return 1 - minimum(input_values)
    elseif kind == :nor
        return 1 - maximum(input_values)
    elseif kind == :sum
        return min(1.0, sum(input_values))
    elseif kind == :avg
        return mean(input_values)
    elseif kind == :product
        return prod(input_values)
    elseif kind == :diff
        if length(input_values) >= 2
            return abs(input_values[1] - input_values[2])
        else
            return input_values[1]
        end
    elseif kind == :max
        return maximum(input_values)
    elseif kind == :min
        return minimum(input_values)
    else
        return 0.5  # Unknown gate type? Safe default.
    end
end

function evaluate_call!(circuit::Circuit, call_gate::CallGate)::Float64
    target_id = call_gate.target_module
    
    # Make sure module exists
    if !haskey(circuit.module_defs, target_id)
        return 0.5  # Module doesn't exist? Neutral output.
    end
    
    mod_def = circuit.module_defs[target_id]
    
    # Parse module if not already parsed
    if mod_def.circuit === nothing
        parse_module!(circuit, target_id)
    end
    
    # Get the module's circuit
    sub_circuit = mod_def.circuit
    
    # Create input values array
    input_values = Vector{Float64}()
    for (local_node, module_input) in call_gate.input_mappings
        # Get value from local node
        if haskey(circuit.nodes, local_node)
            push!(input_values, circuit.nodes[local_node].value)
        else
            push!(input_values, 0.5)  # Missing node? Neutral.
        end
    end
    
    # If this is a memory module, include previous state
    if mod_def.is_memory
        # First input is combined with state
        if !isempty(input_values)
            new_input = mod_def.decay * mod_def.state + (1 - mod_def.decay) * input_values[1]
            input_values[1] = new_input
        else
            input_values = [mod_def.state]  # No input? Use state.
        end
    end
    
    # Set the module's input nodes
    for (i, value) in enumerate(input_values)
        if i <= length(sub_circuit.ordered_nodes)
            node_id = sub_circuit.ordered_nodes[i]
            if haskey(sub_circuit.nodes, node_id)
                sub_circuit.nodes[node_id].value = value
            end
        end
    end
    
    # Evaluate the module
    evaluate!(sub_circuit)
    
    # Get the output
    output = if mod_def.output_node == 0 || !haskey(sub_circuit.nodes, mod_def.output_node)
        0.5  # No output node? Neutral.
    else
        sub_circuit.nodes[mod_def.output_node].value
    end
    
    # Store state for memory modules
    if mod_def.is_memory
        mod_def.state = output
    end
    
    return output
end

function random_genome(min_length::Int=20, max_length::Int=100)::Vector{Union{Int,Char}}
    genome = Vector{Union{Int,Char}}()
    
    # Always start with an M for main module
    push!(genome, 'M')
    
    # Random number of gates
    num_gates = rand(3:15)
    
    # Add gates and connections
    gate_symbols = ['A', 'O', 'N']
    
    for i in 1:num_gates
        # Add a random gate
        gate = rand(gate_symbols)
        push!(genome, gate)
        
        # Add random connections
        num_connections = rand(0:3)
        for j in 1:num_connections
            push!(genome, rand(1:20))  # From
            push!(genome, rand(1:20))  # To
        end
    end
    
    # Maybe add another module
    if rand() < 0.1
        push!(genome, 'M')
        # Add some gates to the new module
        for i in 1:rand(1:5)
            push!(genome, rand(gate_symbols))
            push!(genome, rand(1:10))
            push!(genome, rand(1:10))
        end
    end
    
    return genome
end

function mutate_genome!(genome::Vector{Union{Int,Char}}, mutation_rate::Float64=0.1)
    # Each symbol has a chance to mutate
    for i in 1:length(genome)
        if rand() < mutation_rate
            if genome[i] isa Char
                # If it's a gate letter, change it
                if genome[i] in ['A', 'O', 'N']
                    genome[i] = rand(['A', 'O', 'N'])
                elseif genome[i] in ['M', 'K']
                    # Could change module type
                    genome[i] = rand(['M', 'K'])
                end
            elseif genome[i] isa Int
                # Change number slightly
                genome[i] = max(1, genome[i] + rand(-3:3))
            end
        end
    end
    
    # Module duplication mutation (5% chance)
    if rand() < 0.05
        # Find a module marker
        module_positions = findall(x -> x isa Char && x in ['M', 'K'], genome)
        if !isempty(module_positions)
            pos = rand(module_positions)
            # Find where module ends
            end_pos = findnext(x -> x isa Char && x in ['M', 'K'], genome, pos + 1)
            if end_pos === nothing
                end_pos = length(genome) + 1
            end
            # Copy module
            module_genes = genome[pos:end_pos-1]
            # Insert copy at random position
            insert_pos = rand(1:length(genome))
            for (j, gene) in enumerate(module_genes)
                insert!(genome, insert_pos + j - 1, gene)
            end
        end
    end
    
    # Add new random module (3% chance)
    if rand() < 0.03
        # Create random module
        new_module = Vector{Union{Int,Char}}()
        push!(new_module, 'M')
        for i in 1:rand(1:5)
            push!(new_module, rand(['A', 'O', 'N']))
            push!(new_module, rand(1:10))
            push!(new_module, rand(1:10))
        end
        # Insert at random position
        insert_pos = rand(1:length(genome))
        for (j, gene) in enumerate(new_module)
            insert!(genome, insert_pos + j - 1, gene)
        end
    end
    
    # Delete a module (2% chance)
    if rand() < 0.02 && length(genome) > 10
        module_positions = findall(x -> x isa Char && x in ['M', 'K'], genome)
        if !isempty(module_positions)
            pos = rand(module_positions)
            end_pos = findnext(x -> x isa Char && x in ['M', 'K'], genome, pos + 1)
            if end_pos === nothing
                end_pos = length(genome) + 1
            end
            deleteat!(genome, pos:end_pos-1)
        end
    end
    
    # Add a module call (C) (10% chance)
    if rand() < 0.1
        target_id = rand(1:10)  # Which module to call
        insert_pos = rand(1:length(genome))
        # Add C and target ID
        insert!(genome, insert_pos, target_id)
        insert!(genome, insert_pos, 'C')
        # Add some input mappings
        for i in 1:rand(1:3)
            insert!(genome, insert_pos, rand(1:20))
            insert!(genome, insert_pos, i)
        end
    end
    
    return genome
end
