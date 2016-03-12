package de.fhwedel.antscout
package snippet

import net.liftweb.http.NamedCometActorSnippet

/**
 * Fügt einen AntScoutCometActor zum System hinzu.
 */
class AddAntScoutCometActor extends NamedCometActorSnippet {

  /**
   * Comet-Klasse.
   *
   * @return Comet-Klasse
   */
  def cometClass = "AntScout"

  /**
   * Aktor-Name.
   *
   * @return Aktor-Name
   */
  def name = "antScout"
}