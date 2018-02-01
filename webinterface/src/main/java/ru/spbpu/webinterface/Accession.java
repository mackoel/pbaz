/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

/**
 *
 * @author kkozlov
 */
@Entity
public class Accession {
    @Id
    @GeneratedValue
    private Long num;
	
    private String genotype;

    private String env;

    protected Accession() {
    }

    public Accession(String genotype, String env) {
	this.genotype = genotype;
	this.env = env;
    }

    public Long getNum() {
	return num;
    }

    public String getGenotype() {
	return genotype;
    }

    public String getEnv() {
	return env;
    }

    @Override
    public String toString() {
	return String.format("Accession[num=%d, genotype='%s', env='%s']", num,
			genotype, env);
    }

}
