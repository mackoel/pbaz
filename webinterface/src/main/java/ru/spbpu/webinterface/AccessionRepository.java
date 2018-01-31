/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ru.spbpu.webinterface;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

/**
 *
 * @author kkozlov
 */
public interface AccessionRepository extends JpaRepository<Accession, Long> {

	List<Accession> findByGenotypeStartsWithIgnoreCase(String genotype);
}