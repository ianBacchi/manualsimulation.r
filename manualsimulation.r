simulate_pipeline <- function(instructions, forwarding = FALSE) {
  n_instructions <- length(instructions)
  max_cycles <- n_instructions + 4  # Número máximo de ciclos necessários
  
  # Matriz para armazenar o estado do pipeline
  pipeline <- matrix(NA, nrow = max_cycles, ncol = 5)
  
  # Função para detectar dependências de dados
  has_data_dependency <- function(curr_instr, prev_instr) {
    # Extrair registradores das instruções
    curr_regs <- strsplit(curr_instr, " ")[[1]]
    prev_regs <- strsplit(prev_instr, " ")[[1]]
    
    # Verificar se há dependência
    return(any(curr_regs %in% prev_regs))
  }
  
  # Processar cada instrução
  for (i in 1:n_instructions) {
    if (i == 1) {
      pipeline[1, 1] <- 1
      for (j in 2:5) {
        pipeline[j, j] <- 1
      }
    } else {
      if (forwarding) {
        start_cycle <- max(pipeline[, 5], na.rm = TRUE) + 1
        for (j in 1:5) {
          pipeline[start_cycle + j - 1, j] <- i
        }
      } else {
        # Checar dependências de dados e adicionar bolhas se necessário
        if (i > 1 && has_data_dependency(instructions[i], instructions[i - 1])) {
          start_cycle <- max(pipeline[, 5], na.rm = TRUE) + 2  # Adiciona bolha
        } else {
          start_cycle <- max(pipeline[, 5], na.rm = TRUE) + 1
        }
        for (j in 1:5) {
          pipeline[start_cycle + j - 1, j] <- i
        }
      }
    }
  }
  
  colnames(pipeline) <- c("IF", "ID", "EX", "MEM", "WB")
  rownames(pipeline) <- paste("Ciclo", 1:max_cycles)
  
  return(pipeline)
}

# Instruções de exemplo
instructions <- c("add $t0,$zero,$t1",
                  "lw $s2,0($sp)",
                  "addi $t4,$s2,10",
                  "sll $s0,$s1,2")

# Simulação sem forwarding
cat("Simulação sem forwarding:\n")
print(simulate_pipeline(instructions, forwarding = FALSE))

# Simulação com forwarding
cat("Simulação com forwarding:\n")
print(simulate_pipeline(instructions, forwarding = TRUE))
