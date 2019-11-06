using Gen

######NUOVO MODELLO con inferenza ##########
@gen function burglar_model()

	#prior of the variables
	earthquake =@trace(bernoulli(0.3), :earthquake)

	burglar = @trace(bernoulli(0.2), :burglar)
	
	if(earthquake)
 		radio  = @trace(bernoulli(0.8), :radio )
	else
		radio  = @trace(bernoulli(0.1), :radio )
	end
	if(burglar && earthquake)
		alarm  = @trace(bernoulli(0.9), :alarm )
	elseif(!burglar && earthquake)
		alarm  = @trace(bernoulli(0.4), :alarm )
	else
		alarm  = @trace(bernoulli(0.05), :alarm )
	end
	
	return (alarm ,radio )
end

u = choicemap(:alarm => true, :radio => true)
# Generate an initial trace consistent with the alarm’s sounding:
(tr, _) = generate(burglar_model, (), u)
println(get_choices(tr))
global tot=0

# Run Metropolis-Hastings

trs, weights, lml = Gen.importance_sampling(burglar_model, (), u, 1000)
probBurglar = sum([exp(weights[i]) * trs[i][:burglar] for i=1:1000])



println(probBurglar)
#weight = project(tr, select(:burglar=>100))
weight = project(tr, select(:burglar))
println(exp(weight))

# Print the final trace
println(get_choices(tr))

