# Função para simular o pipeline de execução das instruções MIPS
simulate_pipeline <- function(instrucoes) {
  n_instructions <- length(instrucoes)
  # Matriz para simular o pipeline com 5 estágios (IF, ID, EX, MEM, WB)
  pipeline <- matrix(NA, nrow = n_instructions, ncol = 5)
  
  # Loop para processar cada instrução
  for (i in 1:n_instructions) {
    if (i == 1) {
      # Primeira instrução
      pipeline[i, 1] <- 1  # IF
      pipeline[i, 2] <- NA # ID
      pipeline[i, 3] <- NA # EX
      pipeline[i, 4] <- NA # MEM
      pipeline[i, 5] <- NA # WB
    } else {
      # Verifica dependências de dados com a instrução anterior
      if (has_data_dependency(instrucoes[i], instrucoes[i - 1])) {
        # Adiciona bolha de atraso se houver dependência de dados
        pipeline[i, 1] <- i   # IF
        pipeline[i, 2] <- NA  # ID (bolha de atraso)
        pipeline[i, 3] <- NA  # EX (bolha de atraso)
        pipeline[i, 4] <- NA  # MEM (bolha de atraso)
        pipeline[i, 5] <- NA  # WB (bolha de atraso)
      } else {
        # Não há dependência de dados, avança normalmente
        pipeline[i, 1] <- i   # IF
        pipeline[i, 2] <- i   # ID
        pipeline[i, 3] <- i   # EX
        pipeline[i, 4] <- i   # MEM
        pipeline[i, 5] <- i   # WB
      }
    }
  }
  
  # Retorna a matriz que representa o pipeline
  return(pipeline)
}

# Função para imprimir o resultado do pipeline formatado no formato desejado
print_pipeline_result <- function(pipeline) {
  n_instrucoes <- nrow(pipeline)
  
  # Imprime cabeçalho dos estágios
  cat(sprintf("%-8s", ""), "IF", "ID", "EX", "MEM", "WB")
  cat("\n")
  
  # Imprime cada ciclo
  for (i in 1:n_instrucoes) {
    cat(sprintf("Ciclo %-3d", i))
    for (j in 1:5) {
      cat(sprintf("%-4s", ifelse(is.na(pipeline[i, j]), "NA", as.character(pipeline[i, j]))))
    }
    cat("\n")
  }
}

# Função para verificar dependências de dados entre duas instruções
has_data_dependency <- function(instr1, instr2) {
  # Função auxiliar para extrair registradores de uma instrução
  extract_registers <- function(instr) {
    # Expressão regular para capturar registradores
    regex <- "\\$\\w+"
    # Encontra todos os registradores na instrução usando gregexpr
    matches <- gregexpr(regex, instr)
    # Extrai os registradores encontrados
    regs <- regmatches(instr, matches)[[1]]
    return(regs)
  }
  
  # Extrai registradores de cada instrução
  regs1 <- extract_registers(instr1)
  regs2 <- extract_registers(instr2)
  
  # Verifica se há interseção de registradores
  if (length(intersect(regs1, regs2)) > 0) {
    return(TRUE)  # Há dependência de dados
  } else {
    return(FALSE)  # Não há dependência de dados
  }
}

# Exemplo de uso da função simulate_pipeline
instrucoes <- c(
  "ADD $t0, $t1, $t2",
  "LW $t3, 100($t4)",
  "SUB $t5, $t6, $t7",
  "SW $t8, 200($t9)"
)

resultado_pipeline <- simulate_pipeline(instrucoes)

# Imprime o resultado formatado
print_pipeline_result(resultado_pipeline)
