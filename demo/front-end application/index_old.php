<!DOCTYPE html>
<html>
  <head>
  <title>Domain Specific Induction for Data Wrangling</title>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.1.3/jquery.min.js"></script>
    <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.3/jquery-ui.min.js"></script>
    <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.11.3/themes/smoothness/jquery-ui.css" />

	
	<!-- theme 
	================================================= -->
	<link href='http://fonts.googleapis.com/css?family=Oswald' rel='stylesheet' type='text/css'>
	<link rel="stylesheet" href="css/bootstrap.css">
	<link rel="stylesheet" href="css/bootstrap-responsive.css">
	<link rel="stylesheet" href="css/prettyPhoto.css" />
	<link rel="stylesheet" href="css/flexslider.css" />
	<link rel="stylesheet" href="css/custom-styles.css">
	
	<link rel="shortcut icon" href="img/favicon.ico">
	<link rel="apple-touch-icon" href="img/apple-touch-icon.png">
	<link rel="apple-touch-icon" sizes="72x72" href="img/apple-touch-icon-72x72.png">
	<link rel="apple-touch-icon" sizes="114x114" href="img/apple-touch-icon-114x114.png">
	
	<script src="js/bootstrap.js"></script>
	<script src="js/jquery.prettyPhoto.js"></script>
	<script src="js/jquery.flexslider.js"></script>
	<script src="js/jquery.custom.js"></script>
	
	<!-- ================================== -->
	
	<link rel="stylesheet" href="estilo.css" />

	<script type="text/javascript">
	
		var inputs = [];
		var outputs = [];
		
		$(document).ready(function() {
			
			$("#description select").change(function(){
				if(this.value!=""){					
					$.ajax({
						type: "GET",
						url: "datasets/DemoDataset_"+this.value+".csv",
						dataType: "text",
						success: function(data) {
							processData(data);
							$("#form tbody").empty();
							/* First instance */
							$("#form tbody").append("<tr>"+
														  "<td><input type=\"text\" value="+inputs[0]+"  class=\"input\"/></td>"+
														  "<td><input type=\"text\" value="+outputs[0]+"  class=\"output\"/></td>"+					  
														"</tr>");
							/* Other instance */							
							for(i=1; i<inputs.length; i++){
								$("#form tbody").append("<tr>"+
														  "<td><input type=\"text\" value="+inputs[i]+"  class=\"input\"/></td>"+
														  "<td><input type=\"text\"  class=\"output\"/></td>"+					  
														"</tr>");
							}
						}
					 });
									 
				}				
			});
			
			$("#reset").click(function(){
				$("#form tbody").empty();
				$("#form tbody").append("<tr><td><input type=\"text\" class=\"input\"/></td><td><input type=\"text\" class=\"output\"/></td></tr>"+
										"<tr><td><input type=\"text\" class=\"input\"/></td><td><input type=\"text\" class=\"output\"/></td></tr>"+
										"<tr><td><input type=\"text\" class=\"input\"/></td><td><input type=\"text\" class=\"output\"/></td></tr>"+
										"<tr><td><input type=\"text\" class=\"input\"/></td><td><input type=\"text\" class=\"output\"/></td></tr>"+
										"<tr><td><input type=\"text\" class=\"input\"/></td><td><input type=\"text\" class=\"output\"/></td></tr>");
			});
			
			var radioState;
			$("input[name=domain]").on('click', function() {
				if (radioState === this) {
					this.checked = false;
					radioState = null;
				} else {
					radioState = this;
				}
			});
			
			var index_inputs = {};
			var index_outputs = {};
			var num_examples = 0;
			
			$("#submit").click(function(){
				num_examples = 0;
				
				$("#form form table tbody tr .input").each(function(index){
					
					/* Busco los que están completados input+output y los uso de predicados */
					/* Input completo + output vacío = ejemplo a completar con función */
					/* Otros casos (input+output vacío/input vcío+output completo = errores... vaciar todos los campos */
					//alert("input: "+$(this).val());
					//alert("output: "+$(this).parent().next("td").children(".output").val());
					index_inputs[index] = $(this).val();
					index_outputs[index] = $(this).parent().next("td").children(".output").val();	
					num_examples++;
				});				
				
				var predicate = "f ";
				for(var i=0; i<num_examples; i++)
				{
					if(index_inputs[i]!="" && index_outputs[i]!="") {
						if(i==0){
							predicate = predicate+"\""+index_inputs[i]+"\" == \""+index_outputs[i]+"\" ";
						} else{
							predicate = predicate+"&& f \""+index_inputs[i]+"\" == \""+index_outputs[i]+"\" ";
						}
					}
				}
				predicate+="\n";
						
				
				/* Instances completed and domain selected*/
				var parametros = {
                	"predicate" : predicate,
					"domain" : $("input[name=domain]:checked").val()
        		};
        
        
				
				/* MagicHaskeller */
        $("#modal").removeClass("hide").removeClass("fade");
        
				$.ajax({
					data:  parametros,
					url:   'socket.php',
					type:  'post',
					success:  function (response) {	
						response =  response.trim();						
						if(response!=""){ /* Hay solución */													
							if(response.charAt(0)=="("){ /* Más de una función */
								response = response.substr(10,(response.indexOf(",")-12));
								alert(response);
							} else { /* Solo una función */
								response = response.substr(0,(response.indexOf(","))) + " a ";
								alert(response);
							}						
							
							/* Para cada instancia con input pero sin output, ejecuto la solución */
							var haskell_parametros = {	
								"num_examples": num_examples,
								"inputs": index_inputs,
								"outputs": index_outputs,
								"f": response
							}

							$.ajax({
								data:  haskell_parametros,
								url:   'haskell.php',
								type:  'post',
								dataType: 'json',
								success:  function (haskell_response) {												
									$("#form form table tbody tr .input").each(function(index){		
										var resultado=haskell_response[index].substr((haskell_response[index].indexOf("\""))+1);
										$(this).parent().next("td").children(".output").val(resultado.substr(0,(resultado.length)-1));
										
									});	
                  
                  $("#modal").addClass("fade").addClass("hide");
								}
							});

								
							
						}
					}
				});
			});
			
		});	
		
		
		function processData(allText) {
			var allTextLines = allText.split(/\r\n|\n/);
			var headers = allTextLines[0].split(',');
			inputs = [];
			outputs = [];			

			for (var i=1; i<allTextLines.length; i++) {
				var data = allTextLines[i].split(',');
				if (data.length == headers.length) {					
					inputs.push(data[0]);
					outputs.push(data[1]);					
				}
			}
			 
		}
	</script>
	</head>
	<body class="home">	
  
		<div class="container">		
      
      <!-- Modal -->
      <div class="modal hide fade" id="modal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
          
          <div class="modal-body">
              <p><img src="img/spinner.gif" /></p>
          </div>
         
      </div>
	
			<div class="row header"><!-- Begin Header -->
			
				<!-- Logo
				================================================== -->
				 <div class="span5 logo">
					<span id="logo"><a href="./">Domain Specific Induction for Data Wrangling</a></span><br>
					<!--<h5>Domain Specific Induction for Data Wrangling</h5>-->
				</div>
  
				<!-- Main Navigation
				================================================== -->
				<div class="span7 navigation">
					<div class="navbar hidden-phone">
					
					<ul class="nav">
						<li class=" active"><a href="./">Home</a></li>
						<li><a href="#">About</a></li>
						<li><a href="#">Contact</a></li>
					</ul>
				
				</div>				
				
				<!-- Mobile Nav
				================================================== -->
				<form action="#" id="mobile-nav" class="visible-phone">
					<div class="mobile-nav-select">
					<select onchange="window.open(this.options[this.selectedIndex].value,'_top')">
						<option value="./">Home</option>
						<option value="about.php">About</option>
						<option value="contact.php">Contact</option>
					</select>
					</div>
				</form>
			   
            </div>
		  
		</div><!-- End Header -->
		
		<div class="row"><!-- Start system -->
			<div class="span12" id="introduction">
				<h2>Automating data wrangling process:</h2>
				<p class="lead">Data wrangling includes transformations of data from different formats. 
          Using domain-specific background knowledge with inductive programming systems, we can solve many data wrangling problems automatically from very few examples. 
          <br>
          <a href="#">Read more...</a></p>
			</div> 
			
			<section id="dsi"> 
			
				<div class="span12" id="description"> <!-- Start description -->
					<h6 class="title-bg"> Fill the inputs and some outputs or use a demo dataset: 
						<span> 				
							<select>
							
							<option value=""></option>
							
							<?php 
							
							$dir = 'datasets';
							$datasets  = scandir($dir);
							
							foreach($datasets as $dataset){
								if(substr( $dataset, 0, 1 ) != "."){
									list($beforename, $filename, $extension) = split("[._]", $dataset);
									?>
										<option value="<?=$filename?>"><?=$filename?></option>
									<?php
									
								}
							}
							
							?>
							
							</select>
						</span>
					</h6>
				</div> <!-- End description -->
				
				<div class="span12" id="form"> <!-- Start table -->
				<form>
					<table>
						<thead>
							<tr>
								<th>Input</th>
								<th>Output</th>
							</tr>
						</thead>
						<tbody>
							<tr>
							  <td><input type="text" class="input"/></td>
							  <td><input type="text" class="output"/></td>					  
							</tr>	
							<tr>
							  <td><input type="text" class="input"/></td>
							  <td><input type="text" class="output"/></td>					  
							</tr>	
							<tr>
							  <td><input type="text" class="input"/></td>
							  <td><input type="text" class="output"/></td>					  
							</tr>	
							<tr>
							  <td><input type="text" class="input"/></td>
							  <td><input type="text" class="output"/></td>					  
							</tr>	
							<tr>
							  <td><input type="text" class="input"/></td>
							  <td><input type="text" class="output"/></td>					  
							</tr>						
						</tbody>
					</table>
				</form>
			</div> <!-- End table -->
			
			<div class="span12 well" id="domains"> <!-- Start domains -->
				<h6> Select the domain(s) of the data* <small>(*If no domain is selected, the system will use a general one)</small></h6>
				<div class="span2 switch">
				  <input id="cmn-toggle-dates" class="cmn-toggle cmn-toggle-yes-no" type="radio" name="domain" value="dates"/>
				  <label for="cmn-toggle-dates" data-on="Dates" data-off="Dates"></label>
				</div>
				<div class="span2 switch">
				  <input id="cmn-toggle-emails" class="cmn-toggle cmn-toggle-yes-no" type="radio" name="domain" value="emails"/>
				  <label for="cmn-toggle-emails" data-on="Emails" data-off="Emails"></label>
				</div>
				<div class="span2 switch">
				  <input id="cmn-toggle-names" class="cmn-toggle cmn-toggle-yes-no" type="radio" name="domain" value="names"/>
				  <label for="cmn-toggle-names" data-on="Names" data-off="Names"></label>
				</div>
				<div class="span2 switch">
				  <input id="cmn-toggle-words" class="cmn-toggle cmn-toggle-yes-no" type="radio" name="domain" value="words"/>
				  <label for="cmn-toggle-words" data-on="Words" data-off="Words"></label>
				</div>			
			</div> <!-- End domains -->
			
			<div class="span12" id="buttons"> <!-- Start buttons -->
				<button class="btn" type="button" id="submit"> Wrangling </button>
				<button class="btn btn-inverse" type="button" id="reset"> Reset  </button>
			</div> <!-- End buttons -->
			
			<div class="span12" id="suggestions"> <!-- Start suggestions -->
			<!-- Funciones -->
			</div>  <!-- End suggestions -->
				
			</section>
			
		</div> <!-- End system -->
		  
    
	<!-- container -->
	</div>
  
	<!-- Footer Area
	================================================== -->
  
	<div class="footer-container"><!-- Begin Footer -->
    	<div class="container">
		
		<div class="row"><!-- Begin Sub Footer -->
			<div class="span12 footer-col footer-sub">
				<div class="row no-margin">
					<div class="span6"><span class="left">© 2016-2017 <a href="http://users.dsic.upv.es/~flip/">DMIP group</a>. All rights reserved.</span></div>
					<div class="span6">
						<span class="right">
						<a href="./">Home</a>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;<a href="#">About</a>&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;<a href="#">Contact</a>
						</span>
					</div>
				</div>
			</div>
		</div><!-- End Sub Footer -->
        	
		</div>
    </div><!-- End Footer -->
	
	<!-- Scroll to Top -->  
    <div id="toTop" class="hidden-phone hidden-tablet">Back to Top</div>
	
    
  </body>
</html>