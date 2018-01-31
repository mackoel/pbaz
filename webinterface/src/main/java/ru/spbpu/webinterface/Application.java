/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

/**
 *
 * @author kkozlov
 */
@SpringBootApplication
public class Application {
    private static final Logger log = LoggerFactory.getLogger(Application.class);

	public static void main(String[] args) {
		SpringApplication.run(Application.class);
	}

	@Bean
	public CommandLineRunner loadData(AccessionRepository repository) {
		return (args) -> {
			// save a couple of customers
//			repository.save(new Accession("Jack", "Bauer"));
//			repository.save(new Accession("Chloe", "O'Brian"));
//			repository.save(new Accession("Kim", "Bauer"));
//			repository.save(new Accession("David", "Palmer"));
//			repository.save(new Accession("Michelle", "Dessler"));

			// fetch all customers
			log.info("Accessions found with findAll():");
			log.info("-------------------------------");
			for (Accession accession : repository.findAll()) {
				log.info(accession.toString());
			}
			log.info("");

			// fetch an individual customer by ID
			Accession accession = repository.findOne(1L);
			log.info("Accession found with findOne(1L):");
			log.info("--------------------------------");
			log.info(accession.toString());
			log.info("");

			// fetch customers by last name
			log.info("Accession found with findByGenotypeStartsWithIgnoreCase('Sirna'):");
			log.info("--------------------------------------------");
			for (Accession sirna : repository
					.findByGenotypeStartsWithIgnoreCase("Sirna")) {
				log.info(sirna.toString());
			}
			log.info("");
		};
	}
}
