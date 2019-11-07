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

u = choicemap(:traceyWetGrass => true, :jackWetGrass => false)
# Generate an initial trace consistent with the alarm’s sounding:
(tr, _) = generate(raining_model, (), u)
println(get_choices(tr))


#trs, weights, lml = Gen.importance_sampling(raining_model, (), u, 1000)
#probBurglar = sum([exp(weights[i]) * trs[i][:raining] for i=1:1000])

global tot=0
global totf=0

# Run importance sampling

#trs, weights, lml = Gen.importance_sampling(burglar_model, (), u, 1000)
#probBurglar = sum([exp(weights[i]) * trs[i][:burglar] for i=1:1000])
for j=1:100
	#single samples
	for i=1:100
	  global tr,_ = mh(tr, select(:raining, :sprinkler))
	end
	choices=(get_choices(tr))
	raining = choices[:raining]
#uso global perchè julia ha un problema di scope	
	if (earthquake)
		global tot = tot+1
  	else
       		global totf = totf+1
  	end
end

println("true",tot)
println("false",totf)
println(tot/100)
