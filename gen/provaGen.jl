using Gen
using SoftGlobalScope

######NUOVO MODELLO con inferenza ##########
@gen function burglar_model()

	#prior of the variables
	earthquake =@trace(bernoulli(0.2), :earthquake)
	#println(earthquake)

	burglar = @trace(bernoulli(0.2), :burglar)
	
	if(earthquake)
 		radio  = @trace(bernoulli(0.7), :radio )
	else
		radio  = @trace(bernoulli(0.3), :radio )
	end
	if(burglar && earthquake)
		alarm  = @trace(bernoulli(0.9), :alarm )
	elseif(!burglar && !earthquake)
		alarm  = @trace(bernoulli(0.05), :alarm )
	else
		alarm  = @trace(bernoulli(0.95), :alarm )
	end
	
	return (alarm ,radio )
end

u = choicemap(:alarm => true, :radio => false)
# Generate an initial trace consistent with the alarm’s sounding:

global tr, _ = generate(burglar_model, (), u)
println(get_choices(tr))
global tot=0
global totf=0

# Run importance sampling

#trs, weights, lml = Gen.importance_sampling(burglar_model, (), u, 1000)
#probBurglar = sum([exp(weights[i]) * trs[i][:burglar] for i=1:1000])
for j=1:100
	#single samples
	for i=1:100
	  global tr,_ = mh(tr, select(:earthquake, :burglar))
	end
	choices=(get_choices(tr))
	earthquake = choices[:burglar]
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
#weight = project(tr, select(:burglar=>100))


# Print the final trace


