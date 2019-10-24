using Gen

######NUOVO MODELLO con inferenza ##########
@gen function burglar_model()

	#prior of the variables
	earthquake =@trace(bernoulli(0.3), :earthquake)

	burglar = @trace(bernoulli(0.2), :burglar)

  radio  = @trace(bernoulli(0.8), :radio )

	alarm  = @trace(bernoulli(0.6), :alarm )
	return (alarm ,radio )
end



x = ()                            # empty tuple, foo takes no args
u =choicemap()                   # empty choice map
(tr1, w1) =generate(burglar_model, x, u) #'tr1'is the trace
println(get_choices(tr1))
#non è una necessario hai fini del problema
	# Use'update'to obtain a trace (tr2) with :burglar=false, :d=true.
	argdiff = () # there are no arguments, so argdiff (tr1, w1) =generate(burglar_model, x, u) #'tr1'is the traceis the empty tuple
	u =choicemap((:alarm, true),(:radio, true))
	(tr2, w2, retdiff, v) =update(tr1, x, argdiff, u) 	#ritorna una nuova traccia che sia compatibile con le osservazioni


@gen function new_proposal(previous_trace)							#proposal distribution
  alarm= previous_trace[:alarm]
  radio = previous_trace[:radio]
  p_burglar=(alarm && radio) ? 0.8 : 0.3
  @trace(bernoulli(p_burglar),  :burglar)
end

#ssi sta definendo metropolis_hastings, ma è uguale alla funzione di default
function metro(trace, proposal, proposal_args)
  proposal_args_fwd = (trace, proposal_args...,)
  (u, fwd_score) =propose(proposal, proposal_args_fwd)

  model_args = get_args(trace) # model arguments
  argdiff = map((_) -> NoChange(), model_args) # diffs for arguments
  (new_trace, w, _, v) =update(trace, model_args, argdiff, u)
  proposal_args_rev = (new_trace, proposal_args...,)

  rev_score =assess(proposal, proposal_args_rev, v)
  println(fwd_score,rev_score[1],w)
  alpha = w + rev_score[1] - fwd_score   # accept (return new trace) or reject (return previous trace)

  r = uniform(0, 1)
  return log(r) < alpha ? (new_trace, true) : (trace, false)
end


(new_trace, _) = metropolis_hastings(tr2,new_proposal,())				# crea una seconda traccia conoscendo i valori della traccia precedente. per ogni variabile propone un nuovo
																															#	valore  p(x) preso dalla proposal distribution. Se il rapporto tra p(x) e p(x i-1) è >1 ->
																																# x viene accettato, altrimenti si tiene nella nuova traccia il valore gia presente nella traccia precedente
																																#in questo modo, riesco a fare inferenza sulle variabili

println(new_trace)
choices=get_choices(new_trace)
weight = project(new_trace, select(:burglar))
println(weight)

#Modello da migliorare
