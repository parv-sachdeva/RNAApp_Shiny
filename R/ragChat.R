GROQ_API_URL   <- "https://api.groq.com/openai/v1/chat/completions"
GROQ_MODEL     <- "llama-3.3-70b-versatile"
GROQ_API_KEY   <- Sys.getenv("GROQ_API_KEY")

SYSTEM_PROMPT <- "You are an AI assistant embedded inside an RNA-seq analysis application.
Your ONLY job is to answer questions about the specific analysis results that are provided to you below.
These results come from the user's own uploaded RNA-seq dataset.

Rules you must follow without exception:
1. Only answer questions that are directly about the analysis data shown below.
2. If the user asks anything unrelated to this analysis (general biology, other topics, coding, personal questions, etc.), reply with exactly: \"I can only answer questions about the analysis results currently loaded in this app.\"
3. Never make up gene names, GO terms, or statistics. Only refer to what is in the context.
4. Be concise and scientifically precise.
5. When referencing specific genes or terms, include their key statistics from the context.

--- ANALYSIS CONTEXT ---
{context}
--- END CONTEXT ---"

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ragChatUI <- function(id) {
    ns <- NS(id)
    tabItem(
        tabName = "RAG",
        tags$head(tags$script(HTML(sprintf("
            function scrollChat_%s() {
                var el = document.getElementById('%s');
                if (el) el.scrollTop = el.scrollHeight;
            }
        ", ns(""), ns("chatBox"))))),
        fluidRow(
            box(
                width = 12,
                title = span(icon("robot"), " AI Analysis Assistant"),
                solidHeader = FALSE,
                uiOutput(ns("statusBanner")),
                div(
                    id = ns("chatBox"),
                    style = paste(
                        "height:460px; overflow-y:auto; padding:14px;",
                        "background:#f8f9fa; border-radius:6px;",
                        "border:1px solid #dee2e6; margin-bottom:12px;"
                    ),
                    uiOutput(ns("chatHistory"))
                ),
                fluidRow(
                    column(10,
                        textAreaInput(
                            ns("query"),
                            label   = NULL,
                            placeholder = paste(
                                "Ask about your analysis…",
                                "e.g. “What are the top upregulated genes?”",
                                "or “Which GO terms are enriched?”"
                            ),
                            rows  = 2,
                            width = "100%"
                        )
                    ),
                    column(2,
                        br(),
                        actionButton(
                            ns("send"), "Send",
                            icon   = icon("paper-plane"),
                            status = "primary",
                            class  = "btn-block"
                        )
                    )
                ),
                div(
                    style = "display:flex; justify-content:space-between; align-items:center;",
                    actionButton(ns("clearChat"), "Clear chat",
                                 size = "sm", status = "secondary"),
                    span(style = "font-size:0.78em; color:#999;",
                         "Powered by Groq · llama-3.3-70b-versatile")
                )
            )
        ),
        fluidRow(
            box(
                width      = 12,
                title      = "Suggested questions",
                collapsed  = TRUE,
                collapsible = TRUE,
                fluidRow(
                    lapply(
                        list(
                            list(id = "sq1", label = "What are the top upregulated genes and their fold changes?"),
                            list(id = "sq2", label = "What are the top downregulated genes?"),
                            list(id = "sq3", label = "Summarise the key findings of the differential expression analysis."),
                            list(id = "sq4", label = "Which biological processes are enriched in the upregulated genes?"),
                            list(id = "sq5", label = "Are there any immune or inflammatory pathways enriched?"),
                            list(id = "sq6", label = "What do the GO results suggest about the biology of this comparison?")
                        ),
                        function(q) {
                            column(4,
                                actionButton(
                                    ns(q$id), q$label,
                                    class = "btn btn-outline-secondary btn-block",
                                    style = "text-align:left; white-space:normal; height:auto; margin-bottom:8px;"
                                )
                            )
                        }
                    )
                )
            )
        )
    )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

ragChatServer <- function(id, serverData) {
    moduleServer(id, function(input, output, session) {

        chat <- reactiveValues(messages = list())

        # ---- status banner ------------------------------------------------
        output$statusBanner <- renderUI({
            key <- GROQ_API_KEY
            if (nchar(trimws(key)) == 0) {
                div(
                    class = "alert alert-warning",
                    style = "margin-bottom:10px; font-size:0.88em;",
                    icon("triangle-exclamation"),
                    strong(" GROQ_API_KEY not set."),
                    " Add it to your .Renviron (local) or Posit Connect environment variables."
                )
            } else if (!hasAnalysisData(serverData)) {
                div(
                    class = "alert alert-info",
                    style = "margin-bottom:10px; font-size:0.88em;",
                    icon("circle-info"),
                    " Upload data and run Differential Expression analysis to enable the assistant."
                )
            } else {
                div(
                    class = "alert alert-success",
                    style = "margin-bottom:10px; font-size:0.88em;",
                    icon("circle-check"),
                    " Ready — analysis data loaded. Ask a question below."
                )
            }
        })

        # ---- chat history render ------------------------------------------
        output$chatHistory <- renderUI({
            msgs <- chat$messages
            if (length(msgs) == 0) {
                div(
                    style = "text-align:center; padding:60px; color:#aaa;",
                    icon("comments", style = "font-size:2.5em;"),
                    h5("Ask anything about your analysis",
                       style = "margin-top:12px; color:#bbb;")
                )
            } else {
                tagList(lapply(msgs, renderBubble))
            }
        })

        # ---- send on button -----------------------------------------------
        observeEvent(input$send, {
            q <- trimws(input$query)
            if (nchar(q) == 0) return()
            updateTextAreaInput(session, "query", value = "")
            handleQuery(q, chat, serverData, session)
        })

        # ---- suggested questions ------------------------------------------
        sq_map <- list(
            sq1 = "What are the top upregulated genes and their fold changes?",
            sq2 = "What are the top downregulated genes?",
            sq3 = "Summarise the key findings of the differential expression analysis.",
            sq4 = "Which biological processes are enriched in the upregulated genes?",
            sq5 = "Are there any immune or inflammatory pathways enriched?",
            sq6 = "What do the GO results suggest about the biology of this comparison?"
        )
        lapply(names(sq_map), function(sqid) {
            observeEvent(input[[sqid]], {
                handleQuery(sq_map[[sqid]], chat, serverData, session)
            }, ignoreInit = TRUE)
        })

        # ---- clear --------------------------------------------------------
        observeEvent(input$clearChat, {
            chat$messages <- list()
        })
    })
}

# ---------------------------------------------------------------------------
# Core: build context → call Groq → render
# ---------------------------------------------------------------------------

handleQuery <- function(question, chat, serverData, session) {
    chat$messages <- c(chat$messages, list(list(role = "user", content = question)))

    context <- buildContext(serverData)
    if (is.null(context)) {
        chat$messages <- c(chat$messages, list(list(
            role    = "assistant",
            content = "No analysis data is available yet. Please upload data and run Differential Expression analysis first."
        )))
        return()
    }

    answer <- callGroq(question, context)
    chat$messages <- c(chat$messages, list(list(role = "assistant", content = answer)))

    # scroll chat box to bottom
    session$sendCustomMessage("scrollChat", list())
}


callGroq <- function(question, context) {
    key <- GROQ_API_KEY
    if (nchar(trimws(key)) == 0) {
        return("GROQ_API_KEY is not configured. Please set it in your environment variables.")
    }

    system_msg <- gsub("{context}", context, SYSTEM_PROMPT, fixed = TRUE)

    body <- list(
        model    = GROQ_MODEL,
        messages = list(
            list(role = "system", content = system_msg),
            list(role = "user",   content = question)
        ),
        temperature = 0.2,
        max_tokens  = 1024
    )

    tryCatch({
        resp <- httr::POST(
            url    = GROQ_API_URL,
            httr::add_headers(
                Authorization = paste("Bearer", key),
                `Content-Type` = "application/json"
            ),
            body   = jsonlite::toJSON(body, auto_unbox = TRUE),
            httr::timeout(30)
        )

        if (httr::status_code(resp) != 200) {
            err <- httr::content(resp, "text", encoding = "UTF-8")
            return(paste("Groq API error:", err))
        }

        parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
        parsed$choices$message$content[1]

    }, error = function(e) {
        paste("Request failed:", e$message)
    })
}

# ---------------------------------------------------------------------------
# Context builder — turns live serverData into a text block for the LLM
# ---------------------------------------------------------------------------

buildContext <- function(serverData) {
    if (!hasAnalysisData(serverData)) return(NULL)

    lines <- c()

    # --- sample info -------------------------------------------------------
    if (!is.null(serverData$samples)) {
        s     <- serverData$samples
        groups <- unique(s$group)
        lines <- c(lines,
            "## Dataset",
            paste0("Samples: ", nrow(s)),
            paste0("Groups: ", paste(groups, collapse = ", ")),
            paste0("Genome: ", s$genome[1]),
            ""
        )
    }

    # --- DE results --------------------------------------------------------
    for (comp in names(serverData$comparisons)) {
        de <- serverData$comparisons[[comp]][["DE"]]
        if (is.null(de)) next

        up   <- de[de$type == "Up",   ]
        down <- de[de$type == "Down", ]

        lines <- c(lines,
            paste0("## Differential Expression: ", comp),
            paste0("Total genes tested: ", nrow(de)),
            paste0("Upregulated (significant): ", nrow(up)),
            paste0("Downregulated (significant): ", nrow(down)),
            ""
        )

        if (nrow(up) > 0) {
            top_up <- up[order(up$log2FoldChange, decreasing = TRUE), ][seq_len(min(20, nrow(up))), ]
            lines  <- c(lines, "### Top upregulated genes (by log2FC):")
            lines  <- c(lines, apply(top_up, 1, function(r) {
                sprintf("  %s: log2FC=%.2f, padj=%.2e",
                        r["geneID"],
                        as.numeric(r["log2FoldChange"]),
                        as.numeric(r["padj"]))
            }))
            lines <- c(lines, "")
        }

        if (nrow(down) > 0) {
            top_dn <- down[order(down$log2FoldChange), ][seq_len(min(20, nrow(down))), ]
            lines  <- c(lines, "### Top downregulated genes (by log2FC):")
            lines  <- c(lines, apply(top_dn, 1, function(r) {
                sprintf("  %s: log2FC=%.2f, padj=%.2e",
                        r["geneID"],
                        as.numeric(r["log2FoldChange"]),
                        as.numeric(r["padj"]))
            }))
            lines <- c(lines, "")
        }

        # thresholds used
        padj_t <- serverData$comparisons[[comp]][["padjThreshold"]]
        fc_t   <- serverData$comparisons[[comp]][["fcThreshold"]]
        if (!is.null(padj_t)) lines <- c(lines,
            paste0("Thresholds used — padj < ", padj_t,
                   ", |log2FC| > log2(", fc_t, ")"), "")
    }

    # --- GO results --------------------------------------------------------
    if (!is.null(serverData$goResults) && length(serverData$goResults) > 0) {
        for (comp in names(serverData$goResults)) {
            go_entry <- serverData$goResults[[comp]]
            go_df    <- go_entry$result
            if (is.null(go_df) || nrow(go_df) == 0) next

            go_df  <- go_df[order(go_df$p.adjust), ]
            top_go <- go_df[seq_len(min(20, nrow(go_df))), ]

            lines <- c(lines,
                paste0("## GO Enrichment (", go_entry$analysisType,
                       " / ", go_entry$ont, "): ", comp),
                paste0("Significant GO terms: ", nrow(go_df)),
                "### Top GO terms:"
            )
            lines <- c(lines, apply(top_go, 1, function(r) {
                sprintf("  [%s] %s — GeneRatio=%s, padj=%.2e, genes=%s",
                        r["ID"], r["Description"], r["GeneRatio"],
                        as.numeric(r["p.adjust"]),
                        substr(r["geneID"], 1, 120))
            }))
            lines <- c(lines, "")
        }
    }

    paste(lines, collapse = "\n")
}

hasAnalysisData <- function(serverData) {
    !is.null(serverData$comparisons) &&
    length(serverData$comparisons) > 0 &&
    !is.null(serverData$comparisons[[1]][["DE"]])
}

# ---------------------------------------------------------------------------
# Render a single chat bubble
# ---------------------------------------------------------------------------

renderBubble <- function(msg) {
    if (msg$role == "user") {
        div(style = "display:flex; justify-content:flex-end; margin-bottom:10px;",
            div(
                style = paste(
                    "background:#0d6efd; color:#fff; padding:9px 14px;",
                    "border-radius:18px 18px 4px 18px; max-width:78%;",
                    "font-size:0.9em; line-height:1.45;"
                ),
                msg$content
            )
        )
    } else {
        div(style = "display:flex; align-items:flex-start; margin-bottom:12px;",
            div(style = "margin-right:8px; margin-top:3px; color:#6c757d; font-size:1.1em;",
                icon("robot")),
            div(
                style = paste(
                    "background:#fff; border:1px solid #dee2e6;",
                    "padding:10px 14px; border-radius:4px 18px 18px 18px;",
                    "max-width:82%; font-size:0.9em; line-height:1.5;"
                ),
                HTML(markdownToHTML(msg$content))
            )
        )
    }
}

# Lightweight markdown → HTML (bold, newlines only — no extra deps)
markdownToHTML <- function(txt) {
    txt <- htmltools::htmlEscape(txt)
    txt <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", txt)
    txt <- gsub("\\*(.+?)\\*",       "<em>\\1</em>",         txt)
    txt <- gsub("\n", "<br>", txt, fixed = TRUE)
    txt
}
