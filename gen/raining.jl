using Gen

######NUOVO MODELLO con inferenza ##########
@gen function raining_model()

	#prior of the variables
	sprinkler =@trace(bernoulli(0.3), :sprinkler)

	raining = @trace(bernoulli(0.2), :raining)
	
	if(raining)
 		jackWetGrass  = @trace(bernoulli(0.9), :jackWetGrass )
	else
		jackWetGrass  = @trace(bernoulli(0.2), :jackWetGrass )
	end
	if(raining && sprinkler)
		traceyWetGrass  = @trace(bernoulli(0.9), :traceyWetGrass )
	elseif(!raining && !sprinkler)
		traceyWetGrass  = @trace(bernoulli(0.2), :traceyWetGrass )
	elseif(raining || sprinkler)
		traceyWetGrass  = @trace(bernoulli(0.8), :traceyWetGrass )
	end
	
	return (sprinkler ,raining )
end

u = choicemap(:traceyWetGrass => true, :jackWetGrass => true)
# Generate an initial trace consistent with the alarm’s sounding:
(tr, _) = generate(raining_model, (), u)
println(get_choices(tr))
global tot=0


trs, weights, lml = Gen.importance_sampling(raining_model, (), u, 1000)
probBurglar = sum([exp(weights[i]) * trs[i][:raining] for i=1:1000])


println(tot\1000)
#weight = project(tr, select(:raining=>100))
weight = project(tr, select(:raining))
println(exp(weight))

# Print the final trace
println(get_choices(tr))

