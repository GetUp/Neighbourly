#TODO: class should become a proper sequel model
class ClaimService

  def initialize(db)
    @db = db
  end

  def claims(slugs)
    @db[:claims].
      where('mesh_block_slug IN ?', slugs.map(&:to_s))
  end

  def claim(mesh_block, claimer)
    @db[:claims].insert(mesh_block_slug: mesh_block, mesh_block_claimer: claimer, claim_date: Time.now)
  end

  def unclaim(mesh_block, unclaimer)
    @db[:claims]
    .where(mesh_block_claimer: unclaimer)
    .where(mesh_block_slug: mesh_block)
    .delete
  end

  def admin_unclaim(mesh_block)
    domains = ENV['PRIMARY_DOMAINS'].split(",").map(&:strip)
    domain_conditions = domains.map{ |domain|
      "lower(mesh_block_claimer) ILIKE '%#{domain}'"
    }.join(' OR ')

    @db[:claims]
    .where(mesh_block_slug: mesh_block)
    .where(domain_conditions)
    .delete
  end

  def data_entry_unclaim(mesh_block)
    @db[:claims]
      .where(mesh_block_slug: mesh_block)
      .delete
  end

  private

  def claimer_details(email)
    @db[:users].where(email: email).first
  end

  def get_mesh_block_slugs(mesh_blocks)
    mesh_blocks.map { |mesh_block| mesh_block['_source']['slug'] }
  end
end
